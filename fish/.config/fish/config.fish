fish_add_path /usr/local/bin
fish_add_path /usr/local/sbin
fish_add_path $HOME/bin
fish_add_path $HOME/.local/bin

if test -d $HOME/go/bin
    fish_add_path $HOME/go/bin
end

if test -d $HOME/.cargo/bin
    fish_add_path $HOME/.cargo/bin
end

if status is-interactive
    # Commands to run in interactive sessions can go here
end
