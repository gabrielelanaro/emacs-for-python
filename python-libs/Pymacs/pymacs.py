#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright © 2001, 2002, 2003 Progiciels Bourbeau-Pinard inc.
# François Pinard <pinard@iro.umontreal.ca>, 2001.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

"""\
Interface between Emacs Lisp and Python - Python part.

Emacs may launch this module as a stand-alone program, in which case it
acts as a server of Python facilities for that Emacs session, reading
requests from standard input and writing replies on standard output.
When used in this way, the program is called "the Pymacs helper".

This module may also be usefully imported by those other Python modules.
See the Pymacs documentation (in `README') for more information.
"""

__metaclass__ = type
import os, sys

old_style_exception = not isinstance(Exception, type)

## Python services for Emacs applications.

class Main:
    debug_file = None
    signal_file = None

    def main(self, *arguments):
        """\
Execute Python services for Emacs, and Emacs services for Python.
This program is meant to be called from Emacs, using `pymacs.el'.

Debugging options:
    -d FILE  Debug the protocol to FILE.
    -s FILE  Trace received signals to FILE.

Arguments are added to the search path for Python modules.
"""
        # Decode options.
        arguments = (os.environ.get('PYMACS_OPTIONS', '').split()
                     + list(arguments))
        import getopt
        options, arguments = getopt.getopt(arguments, 'd:s:')
        for option, value in options:
            if option == '-d':
                self.debug_file = value
            elif option == '-s':
                self.signal_file = value
        arguments.reverse()
        for argument in arguments:
            if os.path.isdir(argument):
                sys.path.insert(0, argument)
        # Inhibit signals.
        import signal
        self.original_handler = signal.signal(
                signal.SIGINT, self.interrupt_handler)
        for counter in range(1, signal.NSIG):
            if counter == signal.SIGINT:
                self.original_handler = signal.signal(counter,
                                                      self.interrupt_handler)

            # The following few lines of code are reported to create IO
            # problems within the Pymacs helper itself, so I merely comment
            # them for now, until we know better.

            #else:
            #    try:
            #        signal.signal(counter, self.generic_handler)
            #    except RuntimeError:
            #        pass
        self.inhibit_quit = True
        # Start protocol and services.
        from Pymacs import __version__
        lisp._protocol.send('version', '"%s"' % __version__)
        lisp._protocol.loop()

    def generic_handler(self, number, frame):
        if self.signal_file:
            file(self.signal_file, 'a').write('%d\n' % number)

    def interrupt_handler(self, number, frame):
        if self.signal_file:
            star = (' *', '')[self.inhibit_quit]
            file(self.signal_file, 'a').write('%d%s\n' % (number, star))
        if not self.inhibit_quit:
            self.original_handler(number, frame)

run = Main()
main = run.main

if old_style_exception:
    ProtocolError = 'ProtocolError'
    ZombieError = 'ZombieError'
else:
    class error(Exception): pass
    class ProtocolError(error): pass
    class ZombieError(error): pass

class Protocol:

    # All exec's and eval's triggered from the Emacs side are all executed
    # within the "loop" method below, so all user context is kept as
    # local variables within this single routine.  Different instances
    # of this Protocol class would yield independant evaluation contexts.
    # But in the usual case, there is only one such instance kept within a
    # Lisp_Interface instance, and the "lisp" global variable within this
    # module holds such a Lisp_Interface instance.

    def __init__(self):
        self.freed = []

    def loop(self):
        # The server loop repeatedly receives a request from Emacs and
        # returns a response, which is either the value of the received
        # Python expression, or the Python traceback if an error occurs
        # while evaluating the expression.

        # The server loop may also be executed, as a recursive invocation,
        # in the context of Emacs serving a Python request.  In which
        # case, we might also receive a notification from Emacs telling
        # that the reply has been transmitted, or that an error occurred.
        # A reply notification from Emacs interrupts the loop: the result
        # of this function is then the value returned from Emacs.
        done = False
        while not done:
            try:
                action, text = self.receive()
                if action == 'eval':
                    action = 'return'
                    try:
                        run.inhibit_quit = False
                        value = eval(text)
                    finally:
                        run.inhibit_quit = True
                elif action == 'exec':
                    action = 'return'
                    value = None
                    try:
                        run.inhibit_quit = False
                        exec text
                    finally:
                        run.inhibit_quit = True
                elif action == 'return':
                    done = True
                    try:
                        run.inhibit_quit = False
                        value = eval(text)
                    finally:
                        run.inhibit_quit = True
                elif action == 'raise':
                    action = 'raise'
                    value = 'Emacs: ' + text
                else:
                    if old_style_exception:
                        raise ProtocolError, "Unknown action %r" % action
                    raise ProtocolError("Unknown action %r" % action)
            except KeyboardInterrupt:
                if done:
                    raise
                action = 'raise'
                value = '*Interrupted*'
            except ProtocolError, exception:
                sys.exit("Protocol error: %s\n" % exception)
            except:
                import StringIO, traceback
                buffer = StringIO.StringIO()
                traceback.print_exc(file=buffer)
                action = 'raise'
                value = buffer.getvalue()
            if not done:
                fragments = []
                print_lisp(value, fragments.append, True)
                self.send(action, ''.join(fragments))
        return value

    def receive(self):
        # Receive a Python expression from Emacs, return (ACTION, TEXT).
        prefix = sys.stdin.read(3)
        if not prefix or prefix[0] != '>':
            if old_style_exception:
                raise ProtocolError, "`>' expected."
            raise ProtocolError("`>' expected.")
        while prefix[-1] != '\t':
            character = sys.stdin.read(1)
            if not character:
                if old_style_exception:
                    raise ProtocolError, "Empty stdin read."
                raise ProtocolError("Empty stdin read.")
            prefix += character
        text = sys.stdin.read(int(prefix[1:-1]))
        if run.debug_file is not None:
            file(run.debug_file, 'a').write(prefix + text)
        return text.split(None, 1)

    def send(self, action, text):
        # Send ACTION and its TEXT argument to Emacs.
        if self.freed:
            # All delayed Lisp cleanup is piggied back on the transmission.
            text = ('(free (%s) %s %s)\n'
                    % (' '.join(map(str, self.freed)), action, text))
            self.freed = []
        else:
            text = '(%s %s)\n' % (action, text)
        prefix = '<%d\t' % len(text)
        if run.debug_file is not None:
            file(run.debug_file, 'a').write(prefix + text)
        sys.stdout.write(prefix + text)
        sys.stdout.flush()

def pymacs_load_helper(file_without_extension, prefix):
    # This function imports a Python module, then returns a Lisp expression
    # which, when later evaluated, will install trampoline definitions in
    # Emacs for accessing the Python module facilities.  MODULE may be a
    # full path, yet without the `.py' or `.pyc' extension, in which case
    # the directory is temporarily added to the Python search path for
    # the sole duration of that import.  All defined symbols on the Lisp
    # side have have PREFIX prepended, and have Python underlines in Python
    # turned into dashes.  If PREFIX is None, it then defaults to the base
    # name of MODULE with underlines turned to dashes, followed by a dash.
    directory, module_name = os.path.split(file_without_extension)
    module_components = module_name.split('.')
    if prefix is None:
        prefix = module_components[-1].replace('_', '-') + '-'
    try:
        object = sys.modules.get(module_name)
        if object:
            reload(object)
        else:
            try:
                if directory:
                    sys.path.insert(0, directory)
                object = __import__(module_name)
            finally:
                if directory:
                    del sys.path[0]
            # Whenever MODULE_NAME is of the form [PACKAGE.]...MODULE,
            # __import__ returns the outer PACKAGE, not the module.
            for component in module_components[1:]:
                object = getattr(object, component)
    except ImportError:
        return None
    load_hook = object.__dict__.get('pymacs_load_hook')
    if load_hook:
        load_hook()
    interactions = object.__dict__.get('interactions', {})
    if not isinstance(interactions, dict):
        interactions = {}
    arguments = []
    for name, value in object.__dict__.items():
        if callable(value) and value is not lisp:
            arguments.append(allocate_python(value))
            arguments.append(lisp[prefix + name.replace('_', '-')])
            try:
                interaction = value.interaction
            except AttributeError:
                interaction = interactions.get(value)
            if callable(interaction):
                arguments.append(allocate_python(interaction))
            else:
                arguments.append(interaction)
    if arguments:
        return [lisp.progn,
                [lisp.pymacs_defuns, [lisp.quote, arguments]],
                object]
    return [lisp.quote, object]

def doc_string(object):
    if hasattr(object, '__doc__'):
        return object.__doc__

## Garbage collection matters.

# Many Python types do not have direct Lisp equivalents, and may not be
# directly returned to Lisp for this reason.  They are rather allocated in
# a list of handles, below, and a handle index is used for communication
# instead of the Python value.  Whenever such a handle is freed from the
# Lisp side, its index is added of a freed list for later reuse.

python = []
freed_list = []

def allocate_python(value):
    assert not isinstance(value, str), (type(value), repr(value))
    # Allocate some handle to hold VALUE, return its index.
    if freed_list:
        index = freed_list[-1]
        del freed_list[-1]
        python[index] = value
    else:
        index = len(python)
        python.append(value)
    return index

def free_python(*indices):
    # Return many handles to the pool.
    for index in indices:
        python[index] = None
        freed_list.append(index)

def zombie_python(*indices):
    # Ensure that some handles are _not_ in the pool.
    for index in indices:
        while index >= len(python):
            freed_list.append(len(python))
            python.append(None)
        python[index] = zombie
        freed_list.remove(index)
    # Merely to make `*Pymacs*' a bit more readable.
    freed_list.sort()

def zombie(*arguments):
    # This catch-all function is set as the value for any function which
    # disappeared with a previous Pymacs helper process, so calling
    # such a function from Emacs will trigger a decipherable diagnostic.
    diagnostic = "Object vanished when the Pymacs helper was killed"
    if lisp.pymacs_dreadful_zombies.value():
        if old_style_exception:
            raise ZombieError, diagnostic
        raise ZombieError(diagnostic)
    lisp.message(diagnostic)

## Emacs services for Python applications.

class Let:

    def __init__(self, **keywords):
        # The stack holds (METHOD, DATA) pairs, where METHOD is the expected
        # unbound pop_* method, and DATA holds information to be restored.
        # METHOD may not be bound to the instance, as this would induce
        # reference cycles, and then, __del__ would not be called timely.
        self.stack = []
        self.push(**keywords)

    def __del__(self):
        while self.stack:
            self.stack[-1][0](self)

    def __nonzero__(self):
        # So stylistic `if let:' executes faster.
        return True

    def push(self, **keywords):
        data = []
        for name, value in keywords.items():
            data.append((name, getattr(lisp, name).value()))
            setattr(lisp, name, value)
        self.stack.append((Let.pop, data))
        return self

    def pop(self):
        method, data = self.stack.pop()
        assert method == Let.pop, (method, data)
        for name, value in data:
            setattr(lisp, name, value)

    def push_excursion(self):
        self.stack.append((Let.pop_excursion, (lisp.current_buffer(),
                                               lisp.point_marker(),
                                               lisp.mark_marker())))
        return self

    def pop_excursion(self):
        method, data = self.stack.pop()
        assert method == Let.pop_excursion, (method, data)
        buffer, point_marker, mark_marker = data
        lisp.set_buffer(buffer)
        lisp.goto_char(point_marker)
        lisp.set_mark(mark_marker)
        lisp.set_marker(point_marker, None)
        lisp.set_marker(mark_marker, None)

    def push_match_data(self):
        self.stack.append((Let.pop_match_data, lisp.match_data()))
        return self

    def pop_match_data(self):
        method, data = self.stack.pop()
        assert method == Let.pop_match_data, (method, data)
        lisp.set_match_data(data)

    def push_restriction(self):
        self.stack.append((Let.pop_restriction, (lisp.point_min_marker(),
                                                 lisp.point_max_marker())))
        return self

    def pop_restriction(self):
        method, data = self.stack.pop()
        assert method == Let.pop_restriction, (method, data)
        point_min_marker, point_max_marker = data
        lisp.narrow_to_region(point_min_marker, point_max_marker)
        lisp.set_marker(point_min_marker, None)
        lisp.set_marker(point_max_marker, None)

    def push_selected_window(self):
        self.stack.append((Let.pop_selected_window, lisp.selected_window()))
        return self

    def pop_selected_window(self):
        method, data = self.stack.pop()
        assert method == Let.pop_selected_window, (method, data)
        lisp.select_window(data)

    def push_window_excursion(self):
        self.stack.append((Let.pop_window_excursion,
                           lisp.current_window_configuration()))
        return self

    def pop_window_excursion(self):
        method, data = self.stack.pop()
        assert method == Let.pop_window_excursion, (method, data)
        lisp.set_window_configuration(data)

class Symbol:

    def __init__(self, text):
        self.text = text

    def __repr__(self):
        return 'lisp[%s]' % repr(self.text)

    def __str__(self):
        return '\'' + self.text

    def value(self):
        return lisp._eval(self.text)

    def copy(self):
        return lisp._expand(self.text)

    def set(self, value):
        if value is None:
            lisp._eval('(setq %s nil)' % self.text)
        else:
            fragments = []
            write = fragments.append
            write('(progn (setq %s ' % self.text)
            print_lisp(value, write, True)
            write(') nil)')
            lisp._eval(''.join(fragments))

    def __call__(self, *arguments):
        fragments = []
        write = fragments.append
        write('(%s' % self.text)
        for argument in arguments:
            write(' ')
            print_lisp(argument, write, True)
        write(')')
        return lisp._eval(''.join(fragments))

class Lisp:

    def __init__(self, index):
        self.index = index

    def __del__(self):
        lisp._protocol.freed.append(self.index)

    def __repr__(self):
        return ('lisp(%s)' % repr(lisp('(prin1-to-string %s)' % self)))

    def __str__(self):
        return '(aref pymacs-lisp %d)' % self.index

    def value(self):
        return self

    def copy(self):
        return lisp._expand(str(self))

class Buffer(Lisp):
    pass

    #def write(text):
    #    # So you could do things like
    #    # print >>lisp.current_buffer(), "Hello World"
    #    lisp.insert(text, self)

    #def point(self):
    #    return lisp.point(self)

class List(Lisp):

    def __call__(self, *arguments):
        fragments = []
        write = fragments.append
        write('(%s' % self)
        for argument in arguments:
            write(' ')
            print_lisp(argument, write, True)
        write(')')
        return lisp._eval(''.join(fragments))

    def __len__(self):
        return lisp._eval('(length %s)' % self)

    def __getitem__(self, key):
        value = lisp._eval('(nth %d %s)' % (key, self))
        if value is None and key >= len(self):
            if old_style_exception:
                raise IndexError, key
            raise IndexError(key)
        return value

    def __setitem__(self, key, value):
        fragments = []
        write = fragments.append
        write('(setcar (nthcdr %d %s) ' % (key, self))
        print_lisp(value, write, True)
        write(')')
        lisp._eval(''.join(fragments))

class Table(Lisp):

    def __getitem__(self, key):
        fragments = []
        write = fragments.append
        write('(gethash ')
        print_lisp(key, write, True)
        write(' %s)' % self)
        return lisp._eval(''.join(fragments))

    def __setitem__(self, key, value):
        fragments = []
        write = fragments.append
        write('(puthash ')
        print_lisp(key, write, True)
        write(' ')
        print_lisp(value, write, True)
        write(' %s)' % self)
        lisp._eval(''.join(fragments))

class Vector(Lisp):

    def __len__(self):
        return lisp._eval('(length %s)' % self)

    def __getitem__(self, key):
        return lisp._eval('(aref %s %d)' % (self, key))

    def __setitem__(self, key, value):
        fragments = []
        write = fragments.append
        write('(aset %s %d ' % (self, key))
        print_lisp(value, write, True)
        write(')')
        lisp._eval(''.join(fragments))

class Lisp_Interface:

    def __init__(self):
        self.__dict__['_cache'] = {'nil': None}
        self.__dict__['_protocol'] = Protocol()

    def __call__(self, text):
        return self._eval('(progn %s)' % text)

    def _eval(self, text):
        self._protocol.send('eval', text)
        return self._protocol.loop()

    def _expand(self, text):
        self._protocol.send('expand', text)
        return self._protocol.loop()

    def __getattr__(self, name):
        if name[0] == '_':
            if old_style_exception:
                raise AttributeError, name
            raise AttributeError(name)
        return self[name.replace('_', '-')]

    def __setattr__(self, name, value):
        if name[0] == '_':
            if old_style_exception:
                raise AttributeError, name
            raise AttributeError(name)
        self[name.replace('_', '-')] = value

    def __getitem__(self, name):
        try:
            return self._cache[name]
        except KeyError:
            symbol = self._cache[name] = Symbol(name)
            return symbol

    def __setitem__(self, name, value):
        try:
            symbol = self._cache[name]
        except KeyError:
            symbol = self._cache[name] = Symbol(name)
        symbol.set(value)

lisp = Lisp_Interface()

print_lisp_quoted_specials = {
        '"': '\\"', '\\': '\\\\', '\b': '\\b', '\f': '\\f',
        '\n': '\\n', '\r': '\\r', '\t': '\\t'}

def print_lisp(value, write, quoted):
    if value is None:
        write('nil')
    elif isinstance(value, bool):
        write(('nil', 't')[value])
    elif isinstance(value, int):
        write(repr(value))
    elif isinstance(value, float):
        write(repr(value))
    elif isinstance(value, basestring):
        multibyte = False
        if isinstance(value, unicode):
            try:
                value = value.encode('ASCII')
            except UnicodeEncodeError:
                value = value.encode('UTF-8')
                multibyte = True
        if multibyte:
            write('(decode-coding-string ')
        write('"')
        for character in value:
            special = print_lisp_quoted_specials.get(character)
            if special is not None:
                write(special)
            elif 32 <= ord(character) < 127:
                write(character)
            else:
                write('\\%.3o' % ord(character))
        write('"')
        if multibyte:
            write(' \'utf-8)')
    elif isinstance(value, list):
        if quoted:
            write("'")
        if len(value) == 0:
            write('nil')
        elif len(value) == 2 and value[0] == lisp.quote:
            write("'")
            print_lisp(value[1], write, False)
        else:
            write('(')
            print_lisp(value[0], write, False)
            for sub_value in value[1:]:
                write(' ')
                print_lisp(sub_value, write, False)
            write(')')
    elif isinstance(value, tuple):
        write('[')
        if len(value) > 0:
            print_lisp(value[0], write, False)
            for sub_value in value[1:]:
                write(' ')
                print_lisp(sub_value, write, False)
        write(']')
    elif isinstance(value, Lisp):
        write(str(value))
    elif isinstance(value, Symbol):
        if quoted:
            write("'")
        write(value.text)
    elif callable(value):
        write('(pymacs-defun %d nil)' % allocate_python(value))
    else:
        write('(pymacs-python %d)' % allocate_python(value))

if __name__ == '__main__':
    main(*sys.argv[1:])
