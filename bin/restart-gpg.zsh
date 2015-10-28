#! /usr/bin/env zsh
pkill gpg-agent
source ~/.antigen/repos/https-COLON--SLASH--SLASH-github.com-SLASH-robbyrussell-SLASH-oh-my-zsh.git/plugins/gpg-agent/gpg-agent.plugin.zsh
emacsclient -e \(gpg-agent-reload\ \"$GPG_AGENT_INFO\"\ \"$SSH_AUTH_SOCK\"\)
