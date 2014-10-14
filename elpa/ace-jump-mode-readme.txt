INTRODUCTION


What's this?

It is a minor mode for Emacs. It can help you to move your cursor
to ANY position in emacs by using only 3 times key press.

Where does ace jump mode come from ?

I firstly see such kind of moving style is in a vim plugin called
EasyMotion. It really attract me a lot. So I decide to write
one for Emacs and MAKE IT BETTER.

So I want to thank to :
        Bartlomiej P.   for his PreciseJump
        Kim Silkebækken for his EasyMotion


What's ace-jump-mode ?

ace-jump-mode is an fast/direct cursor location minor mode. It will
create the N-Branch search tree internal and marks all the possible
position with predefined keys in within the whole emacs view.
Allowing you to move to the character/word/line almost directly.


Usage

Add the following code to your init file, of course you can select
the key that you prefer to.
----------------------------------------------------------

ace jump mode major function

(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


enable a more powerful jump back function from ace jump mode

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

If you use viper mode :
(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
If you use evil
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
----------------------------------------------------------

For more information
Intro Doc: https://github.com/winterTTr/ace-jump-mode/wiki
FAQ      : https://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ
