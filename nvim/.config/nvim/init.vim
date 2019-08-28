set sw=2
set ts=2 
set expandtab
set mouse=a
set nowrap
set number
syntax on
set termguicolors
set cursorline
set laststatus=0

call plug#begin('~/.local/share/nvim/plugged')
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'
  Plug 'Yggdroot/indentLine'
  Plug '/usr/local/opt/fzf'
  Plug 'junegunn/fzf.vim'
  Plug 'junegunn/vim-slash'
  Plug 'prabirshrestha/async.vim'
  Plug 'prabirshrestha/vim-lsp'
  Plug 'ntk148v/vim-horizon'
call plug#end()

let g:indentLine_char = '⎸'

if executable('hie-wrapper')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'hie-wrapper',
        \ 'cmd': {server_info->['hie-wrapper']},
        \ 'whitelist': ['haskell'],
        \ })
endif

let g:lsp_auto_enable=0

colorscheme horizon 
