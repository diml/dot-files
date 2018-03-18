# -*- shell-script -*-

# survival bashrc
# ---------------
# Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3

# For non-interactive shells (like emacs).
[[ "$-" != *i* ]] && return

function exists
{
    command -v "$1" &> /dev/null
}

# +------------------------------------------------------------------+
# | Hacks                                                            |
# +------------------------------------------------------------------+

if exists tput; then
    COLUMNS=$(tput cols)
elif [[ -z $COLUMNS ]]; then
    COLUMNS=80
fi
shopt -s checkwinsize

# +------------------------------------------------------------------+
# | Various customization                                            |
# +------------------------------------------------------------------+

# Do not save duplicate lines in history
export HISTCONTROL=ignoredups

# Increase history size
export HISTSIZE=10000000
export HISTFILESIZE=$HISTSIZE

# Do not truncate history file
shopt -s histappend

# Use most as pager
if exists most; then
    export PAGER=most
fi

# Convenience
exists lesspipe && eval "$(lesspipe)"
exists dircolors && eval "$(dircolors --bourne-shell)"

# Completion
for dn in /etc /usr/local/etc; do
    if [[ -f $dn/bash_completion ]]; then
	. $dn/bash_completion
	break
    fi
done

# Editor
exists emacs && export EDITOR="emacs -nw"

# +------------------------------------------------------------------+
# | Utils                                                            |
# +------------------------------------------------------------------+

function parallelize
{
    local count

    if [[ $1 == "-n" ]]; then
        count=$2
        shift 2
    else
        count=$(grep '^processor' /proc/cpuinfo |wc -l)
    fi

    if [[ $(jobs | grep  -v Done | wc -l) -ge $count ]]; then
        # grep -v Done is to get rid off the status reports of terminating jobs
        wait
    fi

    $@ &
}

function uncapitalize
{
    perl -pe 's/\w.+/\l$&/'
}

function capitalize
{
    perl -pe 's/\w.+/\u$&/'
}

# +------------------------------------------------------------------+
# | ACS & colors for the prompt                                      |
# +------------------------------------------------------------------+

smacs='\e(0' # start of acs
rmacs='\e(B' # end of acs

declare -A acs
declare -A col

acs[bssb]=j
acs[ssbb]=k
acs[sbbs]=l
acs[bbss]=m
acs[bbbb]=n
acs[sbsb]=q
acs[bbbs]=t
acs[bsbb]=u
acs[bbsb]=v
acs[sbbb]=w
acs[bsbs]=x

col[black]='\e[30m'
col[red]='\e[31m'
col[green]='\e[32m'
col[yellow]='\e[33m'
col[blue]='\e[34m'
col[magenta]='\e[35m'
col[cyan]='\e[36m'
col[white]='\e[37m'

col[lblack]='\e[1;30m'
col[lred]='\e[1;31m'
col[lgreen]='\e[1;32m'
col[lyellow]='\e[1;33m'
col[lblue]='\e[1;34m'
col[lmagenta]='\e[1;35m'
col[lcyan]=$(echo -e '\e[1;36m')
col[lwhite]='\e[1;37m'

col[reset]='\e[0m'

# +------------------------------------------------------------------+
# | Prompt                                                           |
# +------------------------------------------------------------------+

# Prompt generation after each command
function prompt_command
{
    local status=$?
    local pwd=${PWD/#$HOME/'~'}

    if (( ${#pwd} + 27 > COLUMNS )); then
 	if (( COLUMNS >= 33 )); then
            pwd="..${pwd:${#pwd}+29-COLUMNS}"
        else
            PS1="\$ "
            return
        fi
    fi

    local sm="$smacs"
    local rm="$rmacs"
    local lc="${col[lcyan]}"
    local lm="${col[lmagenta]}"
    local ly="${col[lyellow]}"
    local lg="${col[lgreen]}"
    local lb="${col[lblue]}"
    local lr="${col[lred]}"
    local line=${acs[sbsb]}

    local len=$((COLUMNS - 24))
    local long_line=$(printf "%${len}s" ' ')
    long_line=${long_line// /$line}

    local status_color
    if (( $status == 0 )); then
        status_color=${col[lblack]}
    else
        status_color=${col[lred]}
    fi

    if [[ "$STDREPLAY" = "recording" ]]; then
        lc="${col[lred]}"
    fi

    PS1="$TITLE\
\[$lc$sm$line$rm\]( \[$lm\]\D{%H:%M:%S}\[$lc\] )\[$sm$line$rm\]< \[$ly\]$pwd\[$lc\] >\[$sm$line${long_line:${#pwd}+${#status}}$rm\][ \[$status_color\]$status\[$lc\] ]\[$sm$line$rm\]
\[$lr\]${debian_chroot:+($debian_chroot)}\u\[$lg\]@\[$lb\]\h \[$lg\]\$ \[${col[reset]}\]"
}

# Title for X
case $TERM in
    rxvt*|xterm*)
        TITLE='\[\e];\u@\h:\w\a\]'
        ;;
    *)
        TITLE=''
        ;;
esac

# Regenerate the prompt after each command
PROMPT_COMMAND=prompt_command

# +------------------------------------------------------------------+
# | Aliases                                                          |
# +------------------------------------------------------------------+

alias m='more'
alias grep='grep --color'
alias g='grep'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias la='ls -a'
alias ll='ls -lh'
alias lla='ls -lah'
alias l='ls -s'
alias s='cd ..'
alias du='du -h'
alias df='df -h'

case $(uname) in
    Linux)
	alias ls='ls --color=auto -FC'
	;;
    *)
	alias ls='ls -FGC'
	;;
esac

# +------------------------------------------------------------------+
# | Welcome message                                                  |
# +------------------------------------------------------------------+

date=$(/bin/date +"%R, %A %d %B %Y")
len=$((COLUMNS - 24))
long_line=$(printf "%${len}s" ' ')
long_line=${long_line// /${acs[sbsb]}}
echo -e "\
${col[lcyan]}$smacs${acs[sbbb]}${acs[sbsb]}${long_line:1:${#date}}${acs[sbsb]}${acs[sbbb]}${long_line:${#date}-20}$rmacs
$smacs${acs[bsbs]}$rmacs ${col[lwhite]}$date${col[lcyan]} $smacs${acs[bsbs]}$rmacs
$smacs${acs[bbss]}${acs[sbsb]}${long_line:1:${#date}}${acs[sbsb]}${acs[bssb]}$rmacs${col[reset]}
"
unset len
unset date
unset long_line

if exists fortune; then
    fortune -s 2> /dev/null
    echo
fi

[ -d ~/.local/bin ] && PATH=$HOME/.local/bin:$PATH
PATH=$HOME/bin:$PATH

if [[ -n "$OPAMROOT" ]]; then
    opaminit="$OPAMROOT/opam-init/init.sh"
else
    opaminit="$HOME/.opam/opam-init/init.sh"
fi
[[ -f "$opaminit" ]] && source "$opaminit"
unset opaminit
