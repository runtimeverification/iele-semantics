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
syn keyword ieleVoid       void

" Label
syn match ieleLabel          "[a-zA-Z0-9\\_\\-\\$\\.]\+\s*\:\{0,1\}"

" Call function
syn match ieleFunctionName '@\([a-zA-Z0-9\\_\\-\\$\\.]\+\|\".\+\"\)'

" Variables (Identifiers)
syn match ieleGlobalVariable "@[a-zA-Z0-9\\_\\-\\$\\.]\+"
syn match ieleLocalVariable  "%[a-zA-Z0-9\\_\\-\\$\\.]\+" 

" Keywords
" syn `keyword` doesn't work well, so use `match` instead
" syn keyword ieleKeyword ret revert br
" syn keyword ieleKeyword at load store sload sstore log create copycreate selfdestruct deposit init send gaslimit
" syn keyword ieleKeyword iszero not add sub mul div exp mod addmod mulmod expmod byte sext twos and or xor shift cmp lt le gt ge eq ne sha3
syn match ieleKeywordMatch "\s*\(ret\|revert\|br\)\s\+"
syn match ieleKeywordMatch "\s*\(at\|load\|store\|sload\|sstore\|log\|create\|copycreate\|selfdestruct\|deposit\|init\|send\|gaslimit\)\s\+" 
syn match ieleKeywordMatch "\s*\(iszero\|not\|add\|sub\|mul\|div\|exp\|mod\|addmod\|mulmod\|expmod\|byte\|sext\|twos\|and\|or\|xor\|shift\|cmp\|lt\|le\|gt\|ge\|eq\|ne\|sha3\)\s\+"  

" Contract
syn match ieleContractName   contained '\([a-zA-Z0-9\\_\\-\\$\\.]\+\|\".\+\"\)'
syn match ieleContract       "\(external\s\+\)\{0,1\}contract\s\+" nextgroup=ieleContractName skipwhite 

" Function
syn match ieleFunction       "define\s\+\(public\s\+\)\{0,1\}" nextgroup=ieleFunctionName skipwhite

" Wrap up
let b:current_syntax = "iele"

hi def link ieleTodo             Todo
hi def link ieleComment          Comment 
hi def link ieleBoolean          Boolean
hi def link ieleNumber           Number 
hi def link ieleFloat            Float
hi def link ieleVoid             Constant
hi def link ieleLabel            Label
hi def link ieleFunctionName     Function
hi def link ieleCallFunction     Keyword
hi def link ieleKeywordMatch     Keyword 
hi def link ieleContractName     Type 
hi def link ieleContract         Keyword
hi def link ieleFunction         Keyword


