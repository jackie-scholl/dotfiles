# Defined in /tmp/fish.iOu0oh/fish_greeting.fish @ line 2
function fish_greeting
    if test -z "$IN_NIX_SHELL"
        switch (date +%H)
            case 24 00 01 02  # 11pm-2am
                echo -sn (set_color --bold red) "Please go to sleep immediately, Jackie! 🛑 💤 😠  "
            case 05 06 07 # 5am-8am
                echo -sn (set_color --bold green) "  You're up early, miss! 🌞 "
            case 08 09 10 11  # 8am-noon
                echo -sn (set_color --bold green) "  Good morning, Jackie! 🌞  "
            case 12 13 14  # noon-3pm
                echo -sn (set_color --bold magenta) "  Make sure to have lunch 🌇  "
            case 15 16 17  # 3pm-6pm
                echo -sn (set_color --bold blue) "  We can still get some work done!  "
            case 18 19     # 6pm-8pm
                echo -sn (set_color --bold blue) "  Dinner time!"
            case 20 21 22 23 # 8pm-midnight 
                echo -sn (set_color --bold blue) "  You're up late, miss! Please go to sleep soon"
            case '*' # error
                echo -sn (set_color --bold red) "  Hehe, you missed a number :3 please check ~/.config/fish/functions/fish_greeting.fish "
        end
        echo -s " Welcome to " (set_color --bold blue) "🐟" (set_color normal)
        #echo -s (set_color normal)(set_color green)"It's "(date '+%l:%M %P')". " \
        #        (uptime | tail -c +12)(set_color normal)
    end
end
