#! /usr/bin/env zsh
pkill gpg-agent
gpg-connect-agent /bye
GPG_TTY=$(tty)
export GPG_TTY
unset SSH_AGENT_PID
export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
emacsclient -e \(gpg-agent-reload\ \"$GPG_AGENT_INFO\"\ \"$SSH_AUTH_SOCK\"\)
export GPG_AGENT_INFO
export SSH_AUTH_SOCK
