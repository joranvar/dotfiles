export NIX_PATH=nixpkgs="$HOME/nixpkgs"

source ~/.antigen/antigen.zsh

antigen bundle gpg-agent
antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting
antigen use oh-my-zsh
antigen theme candy
antigen bundle spwhitt/nix-zsh-completions
antigen bundle zsh-users/zsh-completions src
antigen apply
