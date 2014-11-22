;;; kpfonts.el --- AUCTeX style for `kpfonts.sty' version 3.31.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `kpfonts.sty' version 3.31.

;;; Code:

;;; Kpfonts Minor Mode (heavily based on LaTeX Math Minor Mode code)

(defconst LaTeX-kpfonts-default
  '(;; Other Greek Lowercase
    ("o a" "otheralpha" "Other Greek Lowercase" 945) ;; #X03B1
    ("o b" "otherbeta" "Other Greek Lowercase" 946) ;; #X03B2
    ("o g" "othergamma" "Other Greek Lowercase" 947) ;; #X03B3
    ("o d" "otherdelta" "Other Greek Lowercase" 948) ;; #X03B4
    ("o e" "otherepsilon" "Other Greek Lowercase" 1013) ;; #X03F5
    ("o z" "otherzeta" "Other Greek Lowercase" 950) ;; #X03B6
    ("o h" "othereta" "Other Greek Lowercase" 951) ;; #X03B7
    ("o j" "othertheta" "Other Greek Lowercase" 952) ;; #X03B8
    (nil "otheriota" "Other Greek Lowercase" 953) ;; #X03B9
    ("o k" "otherkappa" "Other Greek Lowercase" 954) ;; #X03BA
    ("o l" "otherlambda" "Other Greek Lowercase" 955) ;; #X03BB
    ("o m" "othermu" "Other Greek Lowercase" 956) ;; #X03BC
    ("o n" "othernu" "Other Greek Lowercase" 957) ;; #X03BD
    ("o x" "otherxi" "Other Greek Lowercase" 958) ;; #X03BE
    ("o p" "otherpi" "Other Greek Lowercase" 960) ;; #X03C0
    ("o r" "otherrho" "Other Greek Lowercase" 961) ;; #X03C1
    ("o s" "othersigma" "Other Greek Lowercase" 963) ;; #X03C3
    ("o t" "othertau" "Other Greek Lowercase" 964) ;; #X03C4
    ("o u" "otherupsilon" "Other Greek Lowercase" 965) ;; #X03C5
    ("o f" "otherphi" "Other Greek Lowercase" 981) ;; #X03D5
    ("o q" "otherchi" "Other Greek Lowercase" 967) ;; #X03C7
    ("o y" "otherpsi" "Other Greek Lowercase" 968) ;; #X03C8
    ("o w" "otheromega" "Other Greek Lowercase" 969) ;; #X03C9
    ("o v e" "othervarepsilon" "Other Greek Lowercase" 949) ;; #X03B5
    ("o v j" "othervartheta" "Other Greek Lowercase" 977) ;; #X03D1
    ("o v p" "othervarpi" "Other Greek Lowercase" 982) ;; #X03D6
    ("o v r" "othervarrho" "Other Greek Lowercase" 1009) ;; #X03F1
    ("o v s" "othervarsigma" "Other Greek Lowercase" 962) ;; #X03C2
    ("o v f" "othervarphi" "Other Greek Lowercase" 966) ;; #X03C6
    ;; Slanted Greek Lowercase
    (nil "alphasl" "Slanted Greek Lowercase" 120572) ;; #X1D6FC
    (nil "betasl" "Slanted Greek Lowercase" 120573) ;; #X1D6FD
    (nil "gammasl" "Slanted Greek Lowercase" 120574) ;; #X1D6FE
    (nil "deltasl" "Slanted Greek Lowercase" 120575) ;; #X1D6FF
    (nil "epsilonsl" "Slanted Greek Lowercase" 120598) ;; #X1D716
    (nil "zetasl" "Slanted Greek Lowercase" 120577) ;; #X1D701
    (nil "etasl" "Slanted Greek Lowercase" 120578) ;; #X1D702
    (nil "thetasl" "Slanted Greek Lowercase" 120579) ;; #X1D703
    (nil "iotasl" "Slanted Greek Lowercase" 120580) ;; #X1D704
    (nil "kappasl" "Slanted Greek Lowercase" 120581) ;; #X1D705
    (nil "lambdasl" "Slanted Greek Lowercase" 120582) ;; #X1D706
    (nil "musl" "Slanted Greek Lowercase" 120583) ;; #X1D707
    (nil "nusl" "Slanted Greek Lowercase" 120584) ;; #X1D708
    (nil "xisl" "Slanted Greek Lowercase" 120585) ;; #X1D709
    (nil "pisl" "Slanted Greek Lowercase" 120587) ;; #X1D70B
    (nil "rhosl" "Slanted Greek Lowercase" 120588) ;; #X1D70C
    (nil "sigmasl" "Slanted Greek Lowercase" 120590) ;; #X1D70E
    (nil "tausl" "Slanted Greek Lowercase" 120591) ;; #X1D70F
    (nil "upsilonsl" "Slanted Greek Lowercase" 120592) ;; #X1D710
    (nil "phisl" "Slanted Greek Lowercase" 120601) ;; #X1D719
    (nil "chisl" "Slanted Greek Lowercase" 120594) ;; #X1D712
    (nil "psisl" "Slanted Greek Lowercase" 120595) ;; #X1D713
    (nil "omegasl" "Slanted Greek Lowercase" 120596) ;; #X1D714
    (nil "varepsilonsl" "Slanted Greek Lowercase" 120576) ;; #X1D700
    (nil "varthetasl" "Slanted Greek Lowercase" 120599) ;; #X1D717
    (nil "varpisl" "Slanted Greek Lowercase" 120603) ;; #X1D71B
    (nil "varrhosl" "Slanted Greek Lowercase" 120602) ;; #X1D71A
    (nil "varsigmasl" "Slanted Greek Lowercase" 120589) ;; #X1D70D
    (nil "varphisl" "Slanted Greek Lowercase" 120593) ;; #X1D711
    ;; Upright Greek Lowercase
    (nil "alphaup" "Upright Greek Lowercase" 945) ;; #X03B1
    (nil "betaup" "Upright Greek Lowercase" 946) ;; #X03B2
    (nil "gammaup" "Upright Greek Lowercase" 947) ;; #X03B3
    (nil "deltaup" "Upright Greek Lowercase" 948) ;; #X03B4
    (nil "epsilonup" "Upright Greek Lowercase" 1013) ;; #X03F5
    (nil "zetaup" "Upright Greek Lowercase" 950) ;; #X03B6
    (nil "etaup" "Upright Greek Lowercase" 951) ;; #X03B7
    (nil "thetaup" "Upright Greek Lowercase" 952) ;; #X03B8
    (nil "iotaup" "Upright Greek Lowercase" 953) ;; #X03B9
    (nil "kappaup" "Upright Greek Lowercase" 954) ;; #X03BA
    (nil "lambdaup" "Upright Greek Lowercase" 955) ;; #X03BB
    (nil "muup" "Upright Greek Lowercase" 956) ;; #X03BC
    (nil "nuup" "Upright Greek Lowercase" 957) ;; #X03BD
    (nil "xiup" "Upright Greek Lowercase" 958) ;; #X03BE
    (nil "piup" "Upright Greek Lowercase" 960) ;; #X03C0
    (nil "rhoup" "Upright Greek Lowercase" 961) ;; #X03C1
    (nil "sigmaup" "Upright Greek Lowercase" 963) ;; #X03C3
    (nil "tauup" "Upright Greek Lowercase" 964) ;; #X03C4
    (nil "upsilonup" "Upright Greek Lowercase" 965) ;; #X03C5
    (nil "phiup" "Upright Greek Lowercase" 981) ;; #X03D5
    (nil "chiup" "Upright Greek Lowercase" 967) ;; #X03C7
    (nil "psiup" "Upright Greek Lowercase" 968) ;; #X03C8
    (nil "omegaup" "Upright Greek Lowercase" 969) ;; #X03C9
    (nil "varepsilonup" "Upright Greek Lowercase" 949) ;; #X03B5
    (nil "varthetaup" "Upright Greek Lowercase" 977) ;; #X03D1
    (nil "varpiup" "Upright Greek Lowercase" 982) ;; #X03D6
    (nil "varrhoup" "Upright Greek Lowercase" 1009) ;; #X03F1
    (nil "varsigmaup" "Upright Greek Lowercase" 962) ;; #X03C2
    (nil "varphiup" "Upright Greek Lowercase" 966) ;; #X03C6
    ;; Other Greek Uppercase
    ("o G" "otherGamma" "Other Greek Uppercase" 120548) ;; #X1D6E4
    ("o D" "otherDelta" "Other Greek Uppercase" 120549) ;; #X1D6E5
    ("o J" "otherTheta" "Other Greek Uppercase" 120553) ;; #X1D6E9
    ("o L" "otherLambda" "Other Greek Uppercase" 120556) ;; #X1D6EC
    ("o X" "otherXi" "Other Greek Uppercase" 120559) ;; #X1D6EF
    ("o P" "otherPi" "Other Greek Uppercase" 120561) ;; #X1D6F1
    ("o S" "otherSigma" "Other Greek Uppercase" 120564) ;; #X1D6F4
    ("o U" "otherUpsilon" "Other Greek Uppercase" 120566) ;; #X1D6F6
    ("o F" "otherPhi" "Other Greek Uppercase" 120567) ;; #X1D6F7
    ("o Y" "otherPsi" "Other Greek Uppercase" 120569) ;; #X1D6F9
    ("o W" "otherOmega" "Other Greek Uppercase" 120570) ;; #X1D6FA
    ;; Slanted Greek Uppercase
    (nil "Gammasl" "Slanted Greek Uppercase" 120548) ;; #X1D6E4
    (nil "Deltasl" "Slanted Greek Uppercase" 120549) ;; #X1D6E5
    (nil "Thetasl" "Slanted Greek Uppercase" 120553) ;; #X1D6E9
    (nil "Lambdasl" "Slanted Greek Uppercase" 120556) ;; #X1D6EC
    (nil "Xisl" "Slanted Greek Uppercase" 120559) ;; #X1D6EF
    (nil "Pisl" "Slanted Greek Uppercase" 120561) ;; #X1D6F1
    (nil "Sigmasl" "Slanted Greek Uppercase" 120564) ;; #X1D6F4
    (nil "Upsilonsl" "Slanted Greek Uppercase" 120566) ;; #X1D6F6
    (nil "Phisl" "Slanted Greek Uppercase" 120567) ;; #X1D6F7
    (nil "Psisl" "Slanted Greek Uppercase" 120569) ;; #X1D6F9
    (nil "Omegasl" "Slanted Greek Uppercase" 120570) ;; #X1D6FA
    ;; Upright Greek Uppercase
    (nil "Gammaup" "Upright Greek Uppercase" 915) ;; #X0393
    (nil "Deltaup" "Upright Greek Uppercase" 916) ;; #X0394
    (nil "Thetaup" "Upright Greek Uppercase" 920) ;; #X0398
    (nil "Lambdaup" "Upright Greek Uppercase" 923) ;; #X039B
    (nil "Xiup" "Upright Greek Uppercase" 926) ;; #X039E
    (nil "Piup" "Upright Greek Uppercase" 928) ;; #X03A0
    (nil "Sigmaup" "Upright Greek Uppercase" 931) ;; #X03A3
    (nil "Upsilonup" "Upright Greek Uppercase" 978) ;; #X03D2
    (nil "Phiup" "Upright Greek Uppercase" 934) ;; #X03A6
    (nil "Psiup" "Upright Greek Uppercase" 936) ;; #X03A8
    (nil "Omegaup" "Upright Greek Uppercase" 937) ;; #X03A9
    ;; Integrals
    (nil "varint" "Integrals" nil)
    (nil "variint" "Integrals" nil)
    (nil "variiint" "Integrals" nil)
    (nil "variiiint" "Integrals" nil)
    (nil "varidotsint" "Integrals" nil)
    (nil "oiint" "Integrals" 8751) ;; #X222F
    (nil "ointctrclockwise" "Integrals" 8755) ;; #X2233
    (nil "ointclockwise" "Integrals" nil)
    (nil "sqint" "Integrals" 10774) ;; #X2A16
    (nil "idotsint" "Integrals" nil)
    (nil "oiiint" "Integrals" 8752) ;; #X2230
    (nil "varointctrclockwise" "Integrals" nil)
    (nil "varointclockwise" "Integrals" 8754) ;; #X2232
    (nil "fint" "Integrals" 10767) ;; #X2A0F
    (nil "oiintctrclockwise" "Integrals" nil)
    (nil "varoiintclockwise" "Integrals" nil)
    (nil "oiiintctrclockwise" "Integrals" nil)
    (nil "varoiiintclockwise" "Integrals" nil)
    (nil "oiintclockwise" "Integrals" nil)
    (nil "varoiintctrclockwise" "Integrals" nil)
    (nil "oiiintclockwise" "Integrals" nil)
    (nil "varoiiintctrclockwise" "Integrals" nil)
    (nil "sqiint" "Integrals" nil)
    (nil "sqiiint" "Integrals" nil)
    ;; Mapping
    (nil "mappedfrom" "Mapping" 8612) ;; #X21A4
    (nil "longmappedfrom" "Mapping" 10235) ;; #X27FB
    (nil "Mapsto" "Mapping" 10503) ;; #X2907
    (nil "Longmapsto" "Mapping" 10238) ;; #X27FE
    (nil "Mappedfrom" "Mapping" 10502) ;; #X2906
    (nil "Longmappedfrom" "Mapping" 10237) ;; #X27FD
    (nil "mmapsto" "Mapping" nil)
    (nil "longmmapsto" "Mapping" nil)
    (nil "mmappedfrom" "Mapping" nil)
    (nil "longmmappedfrom" "Mapping" nil)
    (nil "Mmapsto" "Mapping" nil)
    (nil "Longmmapsto" "Mapping" nil)
    (nil "Mmappedfrom" "Mapping" nil)
    (nil "Longmmappedfrom" "Mapping" nil)
    ;; Arrows
    (nil "dashleftarrow" "Arrows" 10510) ;; #X290E
    (nil "dashrightarrow" "Arrows" 10511) ;; #X290F
    (nil "dashleftrightarrow" "Arrows" nil)
    (nil "leftsquigarrow" "Arrows" 8668) ;; #X21DC
    (nil "Nearrow" "Arrows" 8663) ;; #X21D7
    (nil "Searrow" "Arrows" 8664) ;; #X21D8
    (nil "Nwarrow" "Arrows" 8662) ;; #X21D6
    (nil "Swarrow" "Arrows" 8665) ;; #X21D9
    (nil "leadstoext" "Arrows" 12316) ;; #X301C
    (nil "leadsto" "Arrows" 10547) ;; #X2933
    (nil "boxright" "Arrows" nil)
    (nil "Diamondright" "Arrows" nil)
    (nil "circleright" "Arrows" nil)
    (nil "boxleft" "Arrows" nil)
    (nil "Diamondleft" "Arrows" nil)
    (nil "circleleft" "Arrows" nil)
    (nil "boxdotright" "Arrows" nil)
    (nil "Diamonddotright" "Arrows" nil)
    (nil "circledotright" "Arrows" nil)
    (nil "boxdotleft" "Arrows" nil)
    (nil "Diamonddotleft" "Arrows" nil)
    (nil "circledotleft" "Arrows" nil)
    (nil "boxRight" "Arrows" nil)
    (nil "boxLeft" "Arrows" nil)
    (nil "boxdotRight" "Arrows" nil)
    (nil "boxdotLeft" "Arrows" nil)
    (nil "DiamondRight" "Arrows" nil)
    (nil "DiamondLeft" "Arrows" nil)
    (nil "DiamonddotRight" "Arrows" nil)
    (nil "DiamonddotLeft" "Arrows" nil)
    ;; Neg Arrows
    (nil "ntwoheadrightarrow" "Neg Arrows" 10496) ;; #X2900
    (nil "ntwoheadleftarrow" "Neg Arrows" 11060) ;; #X2B34
    ;; Binary Op
    (nil "multimap" "Binary Op" 8888) ;; #X22B8
    (nil "multimapinv" "Binary Op" 10204) ;; #X27DC
    (nil "multimapboth" "Binary Op" 10719) ;; #X29DF
    (nil "multimapdot" "Binary Op" nil)
    (nil "multimapdotinv" "Binary Op" nil)
    (nil "multimapdotboth" "Binary Op" nil)
    (nil "multimapdotbothA" "Binary Op" 8886) ;; #X22B6
    (nil "multimapdotbothB" "Binary Op" 8887) ;; #X22B7
    (nil "multimapbothvert" "Binary Op" nil)
    (nil "multimapdotbothvert" "Binary Op" nil)
    (nil "multimapdotbothAvert" "Binary Op" nil)
    (nil "multimapdotbothBvert" "Binary Op" nil)
    (nil "Wr" "Binary Op" nil)
    (nil "sqcupplus" "Binary Op" nil)
    (nil "sqcapplus" "Binary Op" nil)
    (nil "medcirc" "Binary Op" 9898) ;; #X26AA
    (nil "medbullet" "Binary Op" 9899) ;; #X26AB
    (nil "invamp" "Binary Op" 8523) ;; #X214B
    (nil "Diamonddot" "Binary Op" 10192) ;; #X27D0
    (nil "Diamond" "Binary Op" 9671) ;; #X25C7
    (nil "Diamondblack" "Binary Op" 9670) ;; #X25C6
    (nil "strictif" "Binary Op" 8880) ;; #X22B0
    (nil "strictfi" "Binary Op" 8881) ;; #X22B1
    (nil "strictiff" "Binary Op" nil)
    (nil "circledless" "Binary Op" 10688) ;; #X29C0
    (nil "circledgtr" "Binary Op" 10689) ;; #X29C1
    (nil "circledwedge" "Binary Op" nil)
    (nil "circledvee" "Binary Op" nil)
    (nil "circledbar" "Binary Op" 10678) ;; #X29B6
    (nil "circledbslash" "Binary Op" 10680) ;; #X29B8
    (nil "bignplus" "Binary Op" nil)
    (nil "bigsqcupplus" "Binary Op" nil)
    (nil "bigsqcapplus" "Binary Op" nil)
    (nil "bigsqcap" "Binary Op" 10757) ;; #X2A05
    (nil "varprod" "Binary Op" 10761) ;; #X2A09
    ;; Relational
    (nil "doteq" "Relational" 8784) ;; #X2250
    (nil "VDash" "Relational" 8875) ;; #X22AB
    (nil "VvDash" "Relational" nil)
    (nil "cong" "Relational" 8773) ;; #X2245
    (nil "preceqq" "Relational" 10931) ;; #X2AB3
    (nil "succeqq" "Relational" 10932) ;; #X2AB4
    (nil "coloneqq" "Relational" nil)
    (nil "varparallel" "Relational" 11005) ;; #X2AFD
    (nil "nvarparallel" "Relational" nil)
    (nil "varparallelinv" "Relational" nil)
    (nil "nvarparallelinv" "Relational" nil)
    (nil "colonapprox" "Relational" nil)
    (nil "colonsim" "Relational" nil)
    (nil "Colonapprox" "Relational" nil)
    (nil "Colonsim" "Relational" nil)
    (nil "eqqcolon" "Relational" 8789) ;; #X2255
    (nil "coloneq" "Relational" nil)
    (nil "eqcolon" "Relational" 8761) ;; #X2239
    (nil "Coloneqq" "Relational" 10868) ;; #X2A74
    (nil "Eqqcolon" "Relational" nil)
    ;; Neg Rel
    (nil "nprecsim" "Neg Rel" nil)
    (nil "nsuccsim" "Neg Rel" nil)
    (nil "nlesssim" "Neg Rel" 8820) ;; #X2274
    (nil "ngtrsim" "Neg Rel" 8821) ;; #X2275
    (nil "nlessapprox" "Neg Rel" nil)
    (nil "ngtrapprox" "Neg Rel" nil)
    (nil "npreccurlyeq" "Neg Rel" 8928) ;; #X22E0
    (nil "nsucccurlyeq" "Neg Rel" 8929) ;; #X22E1
    (nil "ngtrless" "Neg Rel" 8825) ;; #X2279
    (nil "nlessgtr" "Neg Rel" 8824) ;; #X2278
    (nil "nbumpeq" "Neg Rel" nil)
    (nil "nBumpeq" "Neg Rel" nil)
    (nil "nbacksim" "Neg Rel" nil)
    (nil "nbacksimeq" "Neg Rel" nil)
    (nil "nasymp" "Neg Rel" 8813) ;; #X226D
    (nil "nequiv" "Neg Rel" 8802) ;; #X2262
    (nil "nsim" "Neg Rel" 8769) ;; #X2241
    (nil "napprox" "Neg Rel" 8777) ;; #X2249
    (nil "nsubset" "Neg Rel" 8836) ;; #X2284
    (nil "nsupset" "Neg Rel" 8837) ;; #X2285
    (nil "nll" "Neg Rel" nil)
    (nil "ngg" "Neg Rel" nil)
    (nil "nthickapprox" "Neg Rel" 8777) ;; #X2249
    (nil "napproxeq" "Neg Rel" nil)
    (nil "nprecapprox" "Neg Rel" nil)
    (nil "nsuccapprox" "Neg Rel" nil)
    (nil "npreceqq" "Neg Rel" nil)
    (nil "nsucceqq" "Neg Rel" nil)
    (nil "nsimeq" "Neg Rel" 8772) ;; #X2244
    (nil "notin" "Neg Rel" 8713) ;; #X2209
    (nil "notni" "Neg Rel" 8716) ;; #X220C
    (nil "nSubset" "Neg Rel" nil)
    (nil "nSupset" "Neg Rel" nil)
    (nil "nsqsubseteq" "Neg Rel" 8930) ;; #X22E2
    (nil "nsqsupseteq" "Neg Rel" 8931) ;; #X22E3
    (nil "nsqsubset" "Neg Rel" nil)
    (nil "nsqsupset" "Neg Rel" nil)
    ;; Delimeters
    (nil "Lbag" "Delimeters" 10181) ;; #X27C5
    (nil "Rbag" "Delimeters" 10182) ;; #X27C6
    (nil "llbracket" "Delimeters" 10214) ;; #X27E6
    (nil "rrbracket" "Delimeters" 10215) ;; #X27E7
    ;; Accents
    (nil "widearc" "Accents" 8978) ;; #X2312
    (nil "widearcarrow" "Accents" 8405) ;; #X20D5
    (nil "wideOarc" "Accents" 8405) ;; #X20D5
    (nil "wideparen" "Accents" 9180) ;; #X23DC
    (nil "widering" "Accents" nil)
    ;; Misc
    ("v 0" "varemptyset" "Misc" 8709) ;; #X2205
    (nil "lJoin" "Misc" 8905) ;; #X22C9
    (nil "rJoin" "Misc" 8906) ;; #X22CA
    (nil "Join" "Misc" 8904) ;; #X22C8
    (nil "openJoin" "Misc" nil)
    (nil "lrtimes" "Misc" nil)
    (nil "opentimes" "Misc" nil)
    (nil "nplus" "Misc" nil)
    (nil "Top" "Misc" 10986) ;; #X2AEA
    (nil "Bot" "Misc" 10987) ;; #X2AEB
    (nil "Perp" "Misc" 10987) ;; #X2AEB
    (nil "boxast" "Misc" nil)
    (nil "boxbslash" "Misc" nil)
    (nil "boxbar" "Misc" nil)
    (nil "boxslash" "Misc" nil)
    (nil "lambdaslash" "Misc" 411) ;; #X019B
    (nil "lambdabar" "Misc" 411) ;; #X019B
    (nil "varclubsuit" "Misc" 9831) ;; #X2667
    (nil "vardiamondsuit" "Misc" 9830) ;; #X2666
    (nil "varheartsuit" "Misc" 9829) ;; #X2665
    (nil "varspadesuit" "Misc" 9828)) ;; #X2664
  "Alist of kpfonts symbols.

Each entry should be a list with upto four elements, KEY, VALUE,
MENU and CHARACTER.

KEY is the key (after `LaTeX-kpfonts-abbrev-prefix') to be
redefined in kpfonts minor mode.  If KEY is nil, the symbol has
no associated keystroke \(it is available in the menu, though\).

VALUE can be a string with the name of the macro to be inserted,
or a function to be called.  The macro must be given without the
leading backslash.

The third element MENU is the name of the submenu where the
command should be added.  MENU can be either a string
\(e.g. \"greek\"\), a list (e.g. \(\"AMS\" \"Delimiters\"\)\) or
nil.  If MENU is nil, no menu item will be created.

The fourth element CHARACTER is a Unicode character position for
menu display.  When nil, no character is shown.

See also `LaTeX-kpfonts-menu'.")

(defvar LaTeX-kpfonts-abbrev-prefix LaTeX-math-abbrev-prefix
  "Prefix key for use in `LaTeX-kpfonts-mode'.
This has to be a string representing a key sequence in a format
understood by the `kbd' macro.  This corresponds to the syntax
usually used in the Emacs and Elisp manuals.")

(defun LaTeX-kpfonts-abbrev-prefix ()
  "Make a key definition from the variable `LaTeX-kpfonts-abbrev-prefix'."
  (if (stringp LaTeX-kpfonts-abbrev-prefix)
      (read-kbd-macro LaTeX-kpfonts-abbrev-prefix)
    LaTeX-kpfonts-abbrev-prefix))

(defvar LaTeX-kpfonts-keymap (make-sparse-keymap)
  "Keymap used for `LaTeX-kpfonts-mode' commands.")

(defvar LaTeX-kpfonts-menu nil
  "Menu containing commands provided by kpfonts LaTeX package.
The menu entries will be generated dynamically, but you can specify
the sequence by initializing this variable.")

;; We set `LaTeX-kpfonts-menu' after its definition because otherwise, resetting
;; AUCTeX with `C-u C-c C-n' would create duplicate entries in menu.
(setq LaTeX-kpfonts-menu
      '("Kpfonts"
	("Insert Font"
	 ["Math Upright"             (TeX-font nil ?\C-h) :keys "C-c C-f C-h"]
	 ["Math Fraktur"             (TeX-font nil ?\C-k) :keys "C-c C-f C-k"]
	 ["Math Script"              (TeX-font nil ?\C-p) :keys "C-c C-f C-p"]
	 ["Slanted Small Caps"       (TeX-font nil ?\C-l) :keys "C-c C-f C-l"]
	 ["Other Small Caps"         (TeX-font nil ?\C-o) :keys "C-c C-f C-o"]
	 ["Other Slanted Small Caps" (TeX-font nil ?\C-q) :keys "C-c C-f C-q"])
	("Replace Font"
	 ["Math Upright"             (TeX-font t ?\C-h) :keys "C-u C-c C-f C-h"]
	 ["Math Fraktur"             (TeX-font t ?\C-k) :keys "C-u C-c C-f C-k"]
	 ["Math Script"              (TeX-font t ?\C-p) :keys "C-u C-c C-f C-p"]
	 ["Slanted Small Caps"       (TeX-font t ?\C-l) :keys "C-u C-c C-f C-l"]
	 ["Other Small Caps"         (TeX-font t ?\C-o) :keys "C-u C-c C-f C-o"]
	 ["Other Slanted Small Caps" (TeX-font t ?\C-q) :keys "C-u C-c C-f C-q"])
	["Delete Font"              (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
	"-"
	("Other Greek Lowercase") ("Slanted Greek Lowercase")
	("Upright Greek Lowercase") ("Other Greek Uppercase")
	("Slanted Greek Uppercase") ("Upright Greek Uppercase") ("Integrals")
	("Mapping") ("Arrows") ("Neg Arrows") ("Binary Op") ("Relational")
	("Neg Rel") ("Delimeters") ("Accents") ("Misc")))

(let ((math (reverse LaTeX-kpfonts-default))
      (map LaTeX-kpfonts-keymap)
      (unicode (and LaTeX-math-menu-unicode (fboundp 'decode-char))))
  (while math
    (let* ((entry (car math))
	   (key (nth 0 entry))
	   (prefix
	    (and unicode
		 (nth 3 entry)))
	   value menu name)
      (setq math (cdr math))
      (if (and prefix
	       (setq prefix (decode-char 'ucs (nth 3 entry))))
	  (setq prefix (concat (string prefix) " \\"))
	(setq prefix "\\"))
      (if (listp (cdr entry))
	  (setq value (nth 1 entry)
		menu (nth 2 entry))
	(setq value (cdr entry)
	      menu nil))
      (if (stringp value)
	  (progn
	    (setq name (intern (concat "LaTeX-kpfonts-" value)))
	    (fset name (list 'lambda (list 'arg) (list 'interactive "*P")
			     (list 'LaTeX-math-insert value 'arg))))
	(setq name value))
      (if key
	  (progn
	    (setq key (cond ((numberp key) (char-to-string key))
			    ((stringp key) (read-kbd-macro key))
			    (t (vector key))))
	    (define-key map key name)))
      (if menu
	  (let ((parent LaTeX-kpfonts-menu))
	    (if (listp menu)
		(progn
		  (while (cdr menu)
		    (let ((sub (assoc (car menu) LaTeX-kpfonts-menu)))
		      (if sub
			  (setq parent sub)
			(setcdr parent (cons (list (car menu)) (cdr parent))))
		      (setq menu (cdr menu))))
		  (setq menu (car menu))))
	    (let ((sub (assoc menu parent)))
	      (if sub
		  (if (stringp value)
		      (setcdr sub (cons (vector (concat prefix value)
						name t)
					(cdr sub)))
		    (error "Cannot have multiple special kpfonts menu items"))
		(setcdr parent
			(cons (if (stringp value)
				  (list menu (vector (concat prefix value)
						     name t))
				(vector menu name t))
			      (cdr parent)))))))))
  ;; Make the kpfonts prefix char available if it has not been used as a prefix.
  (unless (lookup-key map (LaTeX-kpfonts-abbrev-prefix))
    (define-key map (LaTeX-kpfonts-abbrev-prefix) 'self-insert-command)))

(define-minor-mode LaTeX-kpfonts-mode
  "A minor mode with easy access to kpfonts macros.

Easy insertion of kpfonts symbols.  If you give a prefix
argument, the symbols will be surrounded by dollar signs.  The
following commands are defined:

\\{LaTeX-kpfonts-mode-map}"
  nil nil (list (cons (LaTeX-kpfonts-abbrev-prefix) LaTeX-kpfonts-keymap))
  (if LaTeX-kpfonts-mode
      (easy-menu-add LaTeX-kpfonts-mode-menu LaTeX-kpfonts-mode-map)
    (easy-menu-remove LaTeX-kpfonts-mode-menu))
  (TeX-set-mode-name))

(easy-menu-define LaTeX-kpfonts-mode-menu
  LaTeX-kpfonts-mode-map
  "Menu used in kpfonts minor mode."
  LaTeX-kpfonts-menu)

(defvar LaTeX-kpfonts-mode-enable LaTeX-math-mode
  "If non-nil, enable kpfonts minor mode by default.")

(if LaTeX-kpfonts-mode-enable
    (LaTeX-kpfonts-mode))
;;; Kpfonts Minor Mode ends here

;; New fonts by `kpfonts'.
(setq TeX-font-list
      (append
       TeX-font-list
       '(;; Math fonts
	 (?\C-h "" "" "\\mathup{"   "}")
	 (?\C-k "" "" "\\mathfrak{" "}")
	 (?\C-p "" "" "\\mathscr{"  "}")
	 ;; Text fonts
	 (?\C-l "\\textscsl{"      "}")
	 (?\C-o "\\textothersc{"   "}")
	 (?\C-q "\\textotherscsl{" "}"))))

(TeX-add-style-hook
 "kpfonts"
 (lambda ()
   (unless (LaTeX-provided-package-options-member "kpfonts" "notextcomp")
     (TeX-run-style-hooks "textcomp"))
   (unless (LaTeX-provided-package-options-member "kpfonts" "noamsmath")
     (TeX-run-style-hooks "amsmath"))
   (TeX-add-symbols
    ;; Text fonts options
    '("classicstylenums" 1)
    ;; New text commands
    '("scslshape" 0)
    '("otherscshape" 0)
    '("otherscslshape" 0)
    "othertailQ"
    "othertailscq"
    "othertailscslq"
    ;; Variant integrate symbols
    '("D" 1)
    ;; New extensive symbols
    '("widearc" 1)
    '("widearcarrow" 1)
    '("wideOarc" 1)
    '("wideparen" 1)
    '("widering" 1))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textscsl" "{")
				("textothersc" "{")
				("textotherscsl" "{"))
			      'bold-command)
     (font-latex-add-keywords '(("textscsl" "{")
				("textotherscsl" "{"))
			      'italic-command)
     (font-latex-add-keywords '(("scslshape")
				("otherscshape")
				("otherscslshape"))
			      'bold-declaration)
     (font-latex-add-keywords '(("scslshape")
				("otherscslshape"))
			      'italic-declaration)))
 LaTeX-dialect)

(defvar LaTeX-kpfonts-package-options
  '(;; Main global options
    "light" "fulloldstylenums" "fulloldstyle" "fullveryoldstyle"
    ;; Other global options
    "nomath" "notext" "nosf" "nott" "onlyrm" "noamsmath" "notextcomp"
    ;; Text fonts options
    "lighttext" "oldstylenums" "oldstyle" "veryoldstyle" "rmx" "largesmallcaps"
    "easyscsl" "nofligatures" "lightmath"
    ;; Math typesetting options
    "sfmath" "sfmathbb" "rmmathbb" "nomathscript" "mathcalasscript" "classicReIm"
    "uprightRoman" "frenchstyle" "upright" "oldstylenumsmath" "oldstylemath"
    "veryoldstylemath" "narrowiints" "partialup" "widermath" "noDcommand"
    ;; Position of subscripts and superscripts
    "intlimits" "fullintlimits" "nointlimits" "sumlimits" "fullsumlimits"
    "nosumlimits"
    ;; Greek letters in math mode, options
    "uprightgreeks" "slantedGreeks"
    ;; Other `amsmath' options
    "namelimits" "nonamelimits" "leqno" "reqno" "centertags" "tbtags"
    ;; Misc
    "nowarning")
  "Package options for the kpfonts package.")

;; kpfonts.el ends here
