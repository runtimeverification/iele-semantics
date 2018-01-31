" Vim syntax file
" Language: IELE
" Maintainer: Runtime Verification

if exists("b:current_syntax")
	finish
end

" Comments
syn keyword ieleTodo contained TODO FIXME XXX NOTE
syn match   ieleComment "\/\/.*$" contains=ieleTodo,@Spell
syn region  ieleComment start="/\*" end="\*/" contains=ieleTodo,@Spell

" Constants
syn keyword ieleBoolean    true false
syn match   ieleNumber     "\<\(0\o\+\|0[xX]\x\+\|\d\+\)[lL]\=\>"
syn match   ieleFloat      "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
syn region  ieleString     start=+"+ skip=+\\\\\|\\"+ end=+"+ end=+$+ contains=@Spell

" Variables (Identifiers)
syn match ieleVariable "[a-zA-Z0-9\\_\\-\\$\\.]+"
syn keyword ieleGlobalVariable @ nextgroup=ieleGlobalVarName
syn keyword ieleLocalVariable  % nextgroup=ieleLocalVarName

" syn keyword mainKeywords external contract define


" Wrap up
let b:current_syntax = "iele"

hi def link ieleTodo         Todo
hi def link ieleComment      Comment 
hi def link ieleBoolean      Boolean
hi def link ieleNumber       Number 
hi def link ieleFloat        Float
hi def link ieleString       String
hi def link ieleName         Identifier
hi def link ieleGlobalVariable   Identifier
hi def link ieleLocalVariable    Identifier
