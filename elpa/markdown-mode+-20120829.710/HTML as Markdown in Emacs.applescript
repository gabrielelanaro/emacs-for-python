on mktemp()
	return do shell script "mktemp -t md"
end mktemp

try
        -- set appname to name of (info for (path to frontmost application))
   	set tmpfile to mktemp() & ".md"
	set mods to {control down, option down, shift down, command down}
	tell application "System Events"
		keystroke "a" using {command down}
                delay 0.1
		keystroke "c" using {command down}
                delay 0.1
		set txt to Unicode text of (the clipboard as record)
	end tell

	do shell script "pbpaste | ~/.cabal/bin/pandoc --from=html --to=markdown > " & quoted form of tmpfile

        do shell script "echo " & (quoted form of (path to frontmost application as text)) & " > " & tmpfile & ".meta"

        -- do shell script "emacsclient --eval '(setq markdown-paste-app: " & (path to frontmost application as text) & ")'"
        delay 0.1
	tell application "Emacs"
		open (POSIX file tmpfile as string)
		activate
	end tell
end try