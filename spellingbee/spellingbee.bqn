# spellingbee solver

ctr ← "i"
six ← "todrym"
words ← •file.Lines "words_alpha.txt"

solution ← (3<≠∧(∧´(ctr∊⊢))∧(∧´(ctr∾six)∊˜⊢))¨⊸/(words)
pangram ← (7=≠¨(⍷¨solution))/solution
•Show solution
•Show ≢solution
•Show pangram 

# IsLong ← 3≤≠
# HasCtr ← ∧´(ctr)∊⊢
# OnlySix ← ∧´(ctr∾six)∊˜⊢
# solution ← (OnlySix∧HasCtr∧IsLong)¨⊸/ words 

┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬─────────┐
│~ ¬ │! ⎉ │@ ⚇ │# ⍟│$ ◶ │% ⊘ │^ ⎊ │& ⍎ │* ⍕ │( ⟨ │) ⟩ │_ √ │+ ⋆ │Backspace│
│` ˜ │1 ˘ │2 ¨ │3 ⁼ │4 ⌜ │5 ´  │6 ˝ │7   │8 ∞ │9 ¯ │0 • │- ÷ │= × │         │
├────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬──────┤
│Tab    │Q ↙  │W 𝕎  │E ⍷ │R 𝕣 │T ⍋ │Y   │U   │I ⊑ │O ⊒ │P ⍳ │{ ⊣ │} ⊢ │|     │
│       │q ⌽ │w 𝕨 │e ∊ │r ↑ │t ∧ │y   │u ⊔ │i ⊏ │o ⊐ │p π │[ ← │] → │\     │
├─────── ┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴──────┤
│Caps    │A ↖ │S 𝕊 │D   │F 𝔽 │G 𝔾 │H « │J   │K ⌾ │L » │: · │" ˙ │Enter     │
│lock    │a ⍉ │s 𝕤 │d ↕ │f 𝕗 │g 𝕘 │h ⊸ │j ∘ │k ○ │l ⟜ │; ⋄ │' ↩ │          │
├────────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──────────┤
│Shift      │Z ⋈  │X 𝕏 │C  │V ⍒ │B ⌈ │N   │M ≢ │< ≤ │> ≥ │? ⇐ │Shift       │
│           │z ⥊  │x 𝕩 │c ↓ │v ∨ │b ⌊ │n   │m ≡ │, ∾ │. ≍ │/ ≠ │            │
└───────────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────────────┘
                             Space: ‿




# Case conversion utilities
case ← {
    diff ← -´ "Aa"
    Lower ⇐ -⟜diff
    Upper ⇐ Lower⁼
}


filter ← {
    IsLong ⇐(3≤≠)⊸/¨
    HasCtr ⇐ (ctr∊⊢)⊸/¨
    OnlySix ⇐ (∧´((ctr∾six)∊˜⊢))⊸/¨
}

filter.IsLong(50↑words)
filter.HasCtr(50↑words)
filter.OnlySix(50↑words)
solution ← filter.OnlySix(filter.HasCtr(filter.IsLong(words)))
(0≠≠¨solution)/solution


(50↑words)
(3≤≠)¨(50↑words)
((3≤≠)¨(50↑words))/¨(50↑words)
case.Upper(50↑words)
(3≤≠)⊸/¨(50↑words)
(ctr∊⊢)⊸/¨(150↑words)

⍷"aabbccdde"
(∧⍷)¨(50↑words)
∧´"abdeh"∊(ctr∾six)

(∧´((ctr∾six)∊˜⊢))⊸/¨(50↑words)
a ← (∧´((ctr∾six)∊˜⊢))⊸/¨(50↑words)
a
# remove empty sublists

(0≠≠¨a)/a
"abdeh"∊(ctr∾six)



# ∧´x1∊y1	Is x1 a subset of y1?

≠"string"


+´ (⌽2⋆↕8) × "00011000"-'0'


