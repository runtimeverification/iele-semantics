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
syn keyword ieleVoid       void

" Variables (Identifiers)
" syn match   ieleVariable contained "[a-zA-Z0-9\\_\\-\\$](\\.[a-zA-Z0-9\\_\\-\\$])*"
syn match ieleVariable contained "[a-zA-Z0-9\\_\\-\\$\\.]\+"
syn match ieleGlobalVariable "@" nextgroup=ieleVariable
syn match ieleLocalVariable  "%" nextgroup=ieleVariable

" Keywords
syn keyword ieleKeyword ret revert br
syn keyword ieleKeyword call staticcall at load store sload sstore log create copycreate selfdestruct deposit init send gaslimit
syn keyword ieleKeyword iszero not add sub mul div exp mod addmod mulmod expmod byte sext twos and or xor shift cmp lt le gt ge eq ne sha3

" Label
syn match ieleLabel          "[a-zA-Z0-9\\_\\-\\$\\.]\+\:\{0,1\}"

" Contract
syn match ieleContractName   contained "[a-zA-Z0-9\\_\\-\\$\\.]\+"
syn match ieleContract       "\(external\s\+\)\{0,1\}contract\s\+" nextgroup=ieleContractName skipwhite 

" Function
syn match ieleFunction       "define\s\+\(public\s\+\)\{0,1\}" nextgroup=ieleGlobalVariable skipwhite

" Wrap up
let b:current_syntax = "iele"

hi def link ieleTodo         Todo
hi def link ieleComment      Comment 
hi def link ieleBoolean      Boolean
hi def link ieleNumber       Number 
hi def link ieleFloat        Float
hi def link ieleString       String
hi def link ieleVoid         Constant
hi def link ieleVariable     Identifier 
hi def link ieleKeyword      Keyword 
hi def link ieleLabel        Label
hi def link ieleContractName Type 
hi def link ieleContract     Keyword
hi def link ieleFunction     Keyword

