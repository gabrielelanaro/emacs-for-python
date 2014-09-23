#!/usr/bin/env python

"""
Jedi EPC server.

Copyright (C) 2012 Takafumi Arakaki

Author: Takafumi Arakaki <aka.tkf at gmail.com>

This file is NOT part of GNU Emacs.

Jedi EPC server is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Jedi EPC server is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Jedi EPC server.
If not, see <http://www.gnu.org/licenses/>.

"""

import os
import sys
import re
import itertools
import logging
import site

jedi = None  # I will load it later


PY3 = (sys.version_info[0] >= 3)
NEED_ENCODE = not PY3


def jedi_script(source, line, column, source_path):
    if NEED_ENCODE:
        source = source.encode('utf-8')
        source_path = source_path and source_path.encode('utf-8')
    return jedi.Script(source, line, column, source_path or '')


def candidate_symbol(comp):
    """
    Return a character representing completion type.

    :type comp: jedi.api.Completion
    :arg  comp: A completion object returned by `jedi.Script.complete`.

    """
    try:
        return comp.type[0].lower()
    except (AttributeError, TypeError):
        return '?'


def candidates_description(comp):
    """
    Return `comp.description` in an appropriate format.

    * Avoid return a string 'None'.
    * Strip off all newlines. This is required for using
      `comp.description` as candidate summary.

    """
    desc = comp.description
    return _WHITESPACES_RE.sub(' ', desc) if desc and desc != 'None' else ''
_WHITESPACES_RE = re.compile(r'\s+')


def complete(*args):
    reply = []
    for comp in jedi_script(*args).complete():
        reply.append(dict(
            word=comp.word,
            doc=comp.doc,
            description=candidates_description(comp),
            symbol=candidate_symbol(comp),
        ))
    return reply


def get_in_function_call(*args):
    call_def = jedi_script(*args).get_in_function_call()
    if call_def:
        return dict(
            # p.get_code(False) should do the job.  But jedi-vim use replace.
            # So follow what jedi-vim does...
            params=[p.get_code().replace('\n', '') for p in call_def.params],
            index=call_def.index,
            call_name=call_def.call_name,
        )
    else:
        return []  # nil


def _goto(method, *args):
    """
    Helper function for `goto` and `related_names`.

    :arg  method: `jedi.Script.goto` or `jedi.Script.related_names`
    :arg    args: Arguments to `jedi_script`

    """
    # `definitions` is a list. Each element is an instances of
    # `jedi.api_classes.BaseOutput` subclass, i.e.,
    # `jedi.api_classes.RelatedName` or `jedi.api_classes.Definition`.
    definitions = method(jedi_script(*args))
    return [dict(
        column=d.column,
        line_nr=d.line_nr,
        module_path=d.module_path if d.module_path != '__builtin__' else [],
        module_name=d.module_name,
        description=d.description,
    ) for d in definitions]


def goto(*args):
    return _goto(jedi.Script.goto, *args)


def related_names(*args):
    return _goto(jedi.Script.related_names, *args)


def definition_to_dict(d):
    return dict(
        doc=d.doc,
        description=d.description,
        desc_with_module=d.desc_with_module,
        line_nr=d.line_nr,
        column=d.column,
        module_path=d.module_path,
        name=getattr(d, 'name', []),
        full_name=getattr(d, 'full_name', []),
        type=getattr(d, 'type', []),
    )


def get_definition(*args):
    definitions = jedi_script(*args).get_definition()
    return list(map(definition_to_dict, definitions))


def get_names_recursively(definition, parent=None):
    """
    Fetch interesting defined names in sub-scopes under `definition`.

    :type names: jedi.api_classes.Definition

    """
    d = definition_to_dict(definition)
    try:
        d['local_name'] = parent['local_name'] + '.' + d['name']
    except (AttributeError, TypeError):
        d['local_name'] = d['name']
    if definition.type == 'class':
        ds = definition.defined_names()
        return [d] + [get_names_recursively(c, d) for c in ds]
    else:
        return [d]


def defined_names(*args):
    return list(map(get_names_recursively, jedi.api.defined_names(*args)))


def get_module_version(module):
    try:
        from pkg_resources import get_distribution, DistributionNotFound
        try:
            return get_distribution(module.__name__).version
        except DistributionNotFound:
            pass
    except ImportError:
        pass

    notfound = object()
    for key in ['__version__', 'version']:
        version = getattr(module, key, notfound)
        if version is not notfound:
            return version


def get_jedi_version():
    import epc
    import sexpdata
    return [dict(
        name=module.__name__,
        file=getattr(module, '__file__', []),
        version=get_module_version(module) or [],
    ) for module in [sys, jedi, epc, sexpdata]]


def jedi_epc_server(address='localhost', port=0, port_file=sys.stdout,
                    sys_path=[], virtual_env=[],
                    debugger=None, log=None, log_level=None,
                    log_traceback=None):
    add_virtualenv_path()
    for p in virtual_env:
        add_virtualenv_path(p)
    sys_path = map(os.path.expandvars, map(os.path.expanduser, sys_path))
    sys.path = [''] + list(filter(None, itertools.chain(sys_path, sys.path)))
    # Workaround Jedi's module cache.  Use this workaround until Jedi
    # got an API to set module paths.
    # See also: https://github.com/davidhalter/jedi/issues/36
    import_jedi()
    import epc.server
    server = epc.server.EPCServer((address, port))
    server.register_function(complete)
    server.register_function(get_in_function_call)
    server.register_function(goto)
    server.register_function(related_names)
    server.register_function(get_definition)
    server.register_function(defined_names)
    server.register_function(get_jedi_version)

    @server.register_function
    def toggle_log_traceback():
        server.log_traceback = not server.log_traceback
        return server.log_traceback

    port_file.write(str(server.server_address[1]))  # needed for Emacs client
    port_file.write("\n")
    port_file.flush()
    if port_file is not sys.stdout:
        port_file.close()

    # This is not supported Python-EPC API, but I am using this for
    # backward compatibility for Python-EPC < 0.0.4.  In the future,
    # it should be passed to the constructor.
    server.log_traceback = bool(log_traceback)

    if log:
        handler = logging.FileHandler(filename=log, mode='w')
        if log_level:
            log_level = getattr(logging, log_level.upper())
            handler.setLevel(log_level)
            server.logger.setLevel(log_level)
        server.logger.addHandler(handler)
    if debugger:
        server.set_debugger(debugger)
        handler = logging.StreamHandler()
        handler.setLevel(logging.DEBUG)
        server.logger.addHandler(handler)
        server.logger.setLevel(logging.DEBUG)

    server.serve_forever()
    server.logger.info('exit')
    return server


def import_jedi():
    global jedi
    import jedi
    import jedi.api


def add_virtualenv_path(venv=os.getenv('VIRTUAL_ENV')):
    """Add virtualenv's site-packages to `sys.path`."""
    if not venv:
        return
    venv = os.path.abspath(venv)
    path = os.path.join(
        venv, 'lib', 'python%d.%d' % sys.version_info[:2], 'site-packages')
    sys.path.insert(0, path)
    site.addsitedir(path)


def main(args=None):
    import argparse
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawTextHelpFormatter,
        description=__doc__)
    parser.add_argument(
        '--address', default='localhost')
    parser.add_argument(
        '--port', default=0, type=int)
    parser.add_argument(
        '--port-file', '-f', default='-', type=argparse.FileType('wt'),
        help='file to write port on.  default is stdout.')
    parser.add_argument(
        '--sys-path', '-p', default=[], action='append',
        help='paths to be inserted at the top of `sys.path`.')
    parser.add_argument(
        '--virtual-env', '-v', default=[], action='append',
        help='paths to be used as if VIRTUAL_ENV is set to it.')
    parser.add_argument(
        '--log', help='save server log to this file.')
    parser.add_argument(
        '--log-level',
        choices=['CRITICAL', 'ERROR', 'WARN', 'INFO', 'DEBUG'],
        help='logging level for log file.')
    parser.add_argument(
        '--log-traceback', action='store_true', default=False,
        help='Include traceback in logging output.')
    parser.add_argument(
        '--pdb', dest='debugger', const='pdb', action='store_const',
        help='start pdb when error occurs.')
    parser.add_argument(
        '--ipdb', dest='debugger', const='ipdb', action='store_const',
        help='start ipdb when error occurs.')
    ns = parser.parse_args(args)
    jedi_epc_server(**vars(ns))


if __name__ == '__main__':
    main()
