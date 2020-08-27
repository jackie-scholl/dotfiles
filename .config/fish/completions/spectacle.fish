complete -c spectacle -s h -l help               -d 'Displays help on commandline options'
complete -c spectacle -l help-all                -d 'Displays help including Qt specific options.'
complete -c spectacle -s v  -l version           -d 'Displays version information.'
complete -c spectacle -l author                  -d 'Show author information.'
complete -c spectacle -l license                 -d 'Show license information.'
complete -c spectacle -l desktopfile -r -F       -d 'The base file name of the desktop entry for this application.'
complete -c spectacle -s f  -l fullscreen        -d 'Capture the entire desktop (default)'
complete -c spectacle -s m  -l current           -d 'Capture the current monitor'
complete -c spectacle -s a  -l activewindow      -d 'Capture the active window'
complete -c spectacle -s u  -l windowundercursor -d 'Capture the window currently under the cursor, including parents of pop-up menus'
complete -c spectacle -s t  -l transientonly     -d 'Capture the window currently under the cursor, excluding parents of pop-up menus'
complete -c spectacle -s r  -l region            -d 'Capture a rectangular region of the screen'
complete -c spectacle -s g  -l gui               -d 'Start in GUI mode (default)'
complete -c spectacle -s b  -l background        -d 'Take a screenshot and exit without showing the GUI'
complete -c spectacle -s s  -l dbus              -d 'Start in DBus-Activation mode'
complete -c spectacle -s n  -l nonotify          -d 'In background mode, do not pop up a notification when the screenshot is taken'
complete -c spectacle -s o  -l output -r -F      -d 'In background mode, save image to specified file'
complete -c spectacle -s d  -l delay -r -f       -d 'In background mode, delay before taking the shot (in milliseconds)'
complete -c spectacle -s c  -l clipboard         -d 'In background mode, copy screenshot to clipboard'
complete -c spectacle -s w  -l onclick           -d 'Wait for a click before taking screenshot. Invalidates delay'
