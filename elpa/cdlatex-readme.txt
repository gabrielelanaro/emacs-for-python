CDLaTeX is a minor mode supporting fast insertion of environment
templates and math stuff in LaTeX.

To turn CDLaTeX Minor Mode on and off in a particular buffer, use
`M-x cdlatex-mode'.

To turn on CDLaTeX Minor Mode for all LaTeX files, add one of the
following lines to your .emacs file:

  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode

For key bindings, see further down in this documentation.

CDLaTeX requires texmathp.el which is distributed with AUCTeX.
Starting with Emacs 21.3, texmathp.el will be part of Emacs.

--------------------------------------------------------------------------

OVERVIEW
========

CDLaTeX is a minor mode supporting mainly mathematical and scientific
text development with LaTeX.  CDLaTeX is really about speed.  AUCTeX
(the major mode I recommend for editing LaTeX files) does have a hook
based system for inserting environments and macros - but while this is
useful and general, it is sometimes slow to use.  CDLaTeX tries to be
quick, with very few and easy to remember keys, and intelligent
on-the-fly help.

1. ABBREVIATIONS.
   -------------
   CDLaTeX has an abbrev-like mechanism to insert full LaTeX
   environments and other templates into the buffer.  Abbreviation
   expansion is triggered with the TAB key only, not with SPC or RET.
   For example, typing "ite<TAB>" inserts an itemize environment.  A
   full list of defined abbreviations is available with the command
   `C-c ?' (`cdlatex-command-help').

   1a. ENVIRONMENT TEMPLATES
       ---------------------
       Typing `C-c {' (`cdlatex-environment') uses the minibuffer to
       complete the name of a LaTeX environment and inserts a template
       for this environment into the buffer.  These environment
       templates also contain labels created with RefTeX.  In a
       template, text needs to be filled in at various places, which we
       call "points of interest".  You can use the TAB key to jump to
       the next point of interest in the template.

       For many frequently used LaTeX environments, abbreviations are
       available.  Most of the time, the abbreviation consists of the
       first three letters of the environment name: `equ<TAB>' expands
       into
           \begin{equation}
           \label{eq:1}

           \end{equation}

       Similarly, `ali<TAB>' inserts an AMS-LaTeX align environment
       template etc.  For a full list of environment abbreviations, use
       `C-c ?'.

       Use the command `C-c -' (`cdlatex-item') to insert a generalized
       new "item" in any "list"-like environment.  For example, in an
       itemize environment, this inserts "\item", in an enumerate
       environment it inserts "\item\label{item:25}" and in an eqnarray
       environment, it inserts "\label{eq:25} \n & &".  When
       appropriate, newlines are inserted, and the previous item is also
       closed with "\\".  `cdlatex-item' can also be invoked with the
       abbreviation "it<TAB>".

   1b. MATH TEMPLATES
       --------------
       Abbreviations are also used to insert simple math templates
       into the buffer.  The cursor will be positioned properly.  For
       example, typing `fr<TAB>' will insert "\frac{}{}" with the
       cursor in the first pair of parenthesis.  Typing `lr(<TAB>'
       will insert a "\left( \right)" pair and position the cursor in
       between, etc.  Again, the TAB key can be used to jump to the
       points in the template where additional text has to be
       inserted.  For example in the `\frac{}{}' template, it will
       move you from the first argument to the second and then out of
       the second.  For a list of available templates, type `C-c ?'.

2. MATHEMATICAL SYMBOLS
   --------------------
   This feature is similar to the functionality in the Math minor mode
   of AUCTeX, and to the input methods of the X-Symbol package.  It is
   introduced by the backquote character.  Backquote followed by any
   character inserts a LaTeX math macro into the buffer.  If
   necessary, a pair of "$" is inserted to switch to math mode.  For
   example, typing "`a" inserts "$\alpha$".  Since LaTeX defines many
   more mathematical symbols than the alphabet has letters, different
   sets of math macros are provided.  We call the different sets
   "levels".  On each level, another LaTeX macro is assigned to a
   given letter.  To select the different levels, simply press the
   backquote character several times before pressing the letter.  For
   example, typing "`d" inserts "\delta" (level 1), and typing "``d"
   inserts "\partial" (level 2).  Similarly, "`e" inserts "\epsilon"
   and "``e" inserts "\vareppsilon".

   On each level, on-thy-fly help will pop up automatically if you
   hesitate to press the next key.  The help screen is a window which
   lists all math macros available on the current level.  Initially,
   when you type slowly, this window will pop up each time you press
   backquote.  However, after you have learned the different keys, you
   will type more quickly and the help window is not shown.  Try it
   out: First press "`" (backquote), wait for the help window and then
   press "a" to get "\alpha".  Then press "`" and "b" as a quick
   sequence to get "\beta", without the help window.

   The LaTeX macros available through this mechanism are fully
   configurable - see the variable `cdlatex-math-symbol-alist'.

3. ACCENTS AND FONTS
   -----------------
   Putting accents on mathematical characters and/or changing the font
   of a character uses key combinations with the quote character "'"
   as a prefix.  The accent or font change is applied to the character
   or LaTeX macro *before* point.  For example

     Keys                            Result
     --------------------------------------------------------------------
     a'~                             ERROR                 % in text mode
     $a'~                            \tilde{a}             % in math mode
     a':                             \ddot{a}
     ab'b                            \textbf{ab}           % in text mode
     $ab'b                           a\mathbf{b}           % in math mode
     \alpha'.                        \dot{\alpha}
     r_{dust}'r                      r_\mathrm{dust}       % in math mode
     <SPC> 'e                        \emph{}
     this is important   M-2 'b      this \textbf{is important}

   As you can see:
   - using math accents like ~ outside math mode will throw an error.
   - the font change used automatically adapts to math mode.
   - if the item before point is a LaTeX macro, the change applies to
     the whole macro.
   - in text mode, the change applies to the entire word before point,
     while in math mode only the last character is modified.
   - if the character before point is white space, a dollar or an
     opening parenthesis, this command just opens an empty template
     and positions the cursor inside.
   - when a numeric prefix argument is supplied, the command acts on
     whole words before the cursor.

   In order to insert a normal quote, you can press the quote
   character twice.  Also, if the key character is not associated with
   an accent or font, the quote will be inserted.  For example, "'t"
   and "'s" insert just that, so that normal text typing will not be
   disturbed.  Just like during the insertion of math macros (see above
   under (4.)), automatic on-the-fly help will pop up when you pause
   after hitting the quote character, but will be suppressed when you
   continue quickly.  The available accents and also the prefix key
   can be can be configured - see documentation of the variables
   `cdlatex-math-modify-alist' and `cdlatex-math-modify-prefix'.

4. PAIR INSERTION of (), [], {}, and $$
   ------------------------------------
   Dollars and parens can be inserted as pairs.  When you type the
   opening delimiter, the closing delimiter will be inserted as well,
   and the cursor positioned between them.  You can configure which
   delimiter are inserted pairwise by configuring the variable
   `cdlatex-paired-parens'.

   Also, the keys `_' and `^' will insert "_{}" and "^{}",
   respectively, and, if necessary, also a pair of dollar signs to
   switch to math mode.  You can use TAB to exit paired parenthesis.
   As a special case, when you use TAB to exit a pair of braces that
   belong to a subscript or superscript, CDLaTeX removes the braces if
   the sub/superscript consists of a single character.  For example
   typing "$10^3<TAB>" inserts "$10^3$", but typing "$10^34<TAB>"
   inserts "$10^{34}$"

5. THE OVERLOADED TAB KEY
   ----------------------
   You may have noticed that we use the TAB key for many different
   purposes in this package.  While this may seem confusing, I have
   gotten used to this very much.  Hopefully this will work for you as
   well: "when in doubt, press TAB".  Here is a summary of what happens
   when you press the TAB key:

   The function first tries to expand any abbreviation before point.

   If there is none, it cleans up short subscripts and superscripts at
   point.  I.e., is the cursor is just before the closing brace in
   "a^{2}", it changes it to "a^2", since this is more readable.  If
   you want to keep the braces also for simple superscripts and
   subscripts, set the variable `cdlatex-simplify-sub-super-scripts'
   to nil.

   After that, the TAB function jumps to the next point of interest in
   a LaTeX text where one would reasonably expect that more input can
   be put in.  This does *not* use special markers in the template,
   but a heuristic method which works quite well.  For the detailed
   rules which govern this feature, check the documentation of the
   function `cdlatex-tab'.

-----------------------------------------------------------------------------

CONFIGURATION EXAMPLES
======================

Check out the documentation of the variables in the configuration
section.  The variables must be set before cdlatex-mode is turned on,
or, at the latext, in `cdlatex-mode-hook', in order to be effective.
When changing the variables, toggle the mode off and on to make sure
that everything is up to date.

Here is how you might configure CDLaTeX to provide environment templates
(including automatic labels) for two theorem-like environments.

  (setq cdlatex-env-alist
     '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
       ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)))

The "AUTOLABEL" indicates the place where an automatic label should be
inserted, using RefTeX.  The question mark defines the position of the
cursor after the template has been inserted into the buffer.

You could also define your own keyword commands "axm" and "thr" to make
the template insertion quicker (e.g. `axm<TAB>' and `thm<TAB>'):

(setq cdlatex-command-alist
 '(("axm" "Insert axiom env"   "" cdlatex-environment ("axiom") t nil)
   ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)))

Here is how to add new math symbols to CDLaTeX's list: In order to put
all rightarrow commands onto `>, ``>, ```>, and ````> (i.e. several
backquotes followed by >) and all leftarrow commands onto '<, ``<, ```<,
and ````<,  you could do this in .emacs:

  (setq cdlatex-math-symbol-alist
'((?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
  (?> ("\\rightarrow" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))
   ))

To change the prefix key for math accents and font switching, you could
do something like

  (setq cdlatex-math-modify-prefix [f7])
-----------------------------------------------------------------------------

KEY BINDINGS

Here is the default set of keybindings from CDLaTeX.  A menu is also
installed.

  $         cdlatex-dollar
  (         cdlatex-pbb
  {         cdlatex-pbb
  [         cdlatex-pbb
  |         cdlatex-pbb
  <         cdlatex-pbb
  ^         cdlatex-sub-superscript
  _         cdlatex-sub-superscript

  TAB       cdlatex-tab
  C-c ?     cdlatex-command-help
  C-c {     cdlatex-environment
  C-c -     cdlatex-item
  `         cdlatex-math-symbol
  '         cdlatex-math-modify



FAQ

- Some people find it disturbing that the quote character (') is active
  for math accents and font switching.  I have tried to avoid any letters
  which are frequently following ' in normal text.  For example, 's and 't
  insert just this.  If you still prefer a different prefix key, just
  configure the variable `cdlatex-math-modify-prefix'.

- To insert a backquote into the buffer, use C-q `





