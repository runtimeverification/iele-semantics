IELE syntax highlighter for vim

To install, copy (or link) "iele.vim" file into the ~/.vim/syntax 
directory and add the following to your ~/.vimrc file

au BufRead,BufNewFile *.iele set filetype=iele
au! Syntax iele source iele.vim
syn on

