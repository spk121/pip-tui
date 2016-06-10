(define-module (pip-tui pip-colors)
  #:use-module (ncurses curses)
  #:use-module (pip-tui pip-color-names)
  #:export (
	    pip-green-level->color-index
	    
	    color-index-get-brightness
	    pip-green-level-get-brightness
	    pip-green-level-from-brightness

	    color-indices-get-color-pair-index
	    pip-green-levels-get-color-pair-index
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are the xterm 256 color palette

(define *xterm-colors*
  '(
    (0  #x000000)
    (1  #x800000)
    (2  #x008000)			; DefaultGreen
    (3  #x808000)
    (4  #x000080)
    (5  #x800080)
    (6  #x008080)
    (7  #xc0c0c0)			; Default White

    (8  #x808080)
    (9  #xff0000)
    (10  #x00ff00)			; Default Bright Green
    (11  #xffff00)
    (12  #x0000ff)
    (13  #xff00ff)
    (14  #x00ffff)
    (15  #xffffff)			; Default Bright White

    (16  #x000000)			; black
    (17  #x00005f)
    (18  #x000087)			; dark blue
    (19  #x0000af)
    (20  #x0000d7)			; medium blue
    (21  #x0000ff)			; blue
    (22  #x005f00)			; Green5F, Green5, dark green
    (23  #x005f5f)
    (24  #x005f87)
    (25  #x005faf)
    (26  #x005fd7)
    (27  #x005fff)
    (28  #x008700)			; Green87, Green4, green4
    (29  #x00875f)
    (30  #x008787)			; dark cyan
    (31  #x0087af)
    (32  #x0087d7)
    (33  #x0087ff)
    (34  #x00af00)			; GreenAF, Green3
    (35  #x00af5f)
    (36  #x00af87)
    (37  #x00afaf)
    (38  #x00afd7)
    (39  #x00afff)
    (40  #x00d700)			; GreenD7, Green2
    (41  #x00d75f)
    (42  #x00d787)
    (43  #x00d7af)
    (44  #x00d7d7)			; dark turquoise
    (45  #x00d7ff)
    (46  #x00ff00)			; GreenFF, Green1, green
    (47  #x00ff5f)
    (48  #x00ff87)			; spring green
    (49  #x00ffaf)
    (50  #x00ffd7)
    (51  #x00ffff)			; cyan
    (52  #x5f0000)
    (53  #x5f005f)
    (54  #x5f0087)
    (55  #x5f00af)
    (56  #x5f00d7)
    (57  #x5f00ff)
    (58  #x5f5f00)
    (59  #x5f5f5f)
    (60  #x5f5f87)
    (61  #x5f5faf)
    (62  #x5f5fd7)
    (63  #x5f5fff)
    (64  #x5f8700)
    (65  #x5f875f)
    (66  #x5f8787)
    (67  #x5f87af)
    (68  #x5f87d7)
    (69  #x5f87ff)
    (70  #x5faf00)
    (71  #x5faf5f)
    (72  #x5faf87)
    (73  #x5fafaf)
    (74  #x5fafd7)
    (75  #x5fafff)
    (76  #x5fd700)
    (77  #x5fd75f)
    (78  #x5fd787)
    (79  #x5fd7af)
    (80  #x5fd7d7)
    (81  #x5fd7ff)
    (82  #x5fff00)
    (83  #x5fff5f)			; LightGreen5F, LightGreen4
    (84  #x5fff87)
    (85  #x5fffaf)
    (86  #x5fffd7)
    (87  #x5fffff)
    (88  #x870000)			; dark red
    (89  #x87005f)
    (90  #x870087)			; dark magenta
    (91  #x8700af)
    (92  #x8700d7)
    (93  #x8700ff)
    (94  #x875f00)
    (95  #x875f5f)
    (96  #x875f87)
    (97  #x875faf)
    (98  #x875fd7)
    (99  #x875fff)
    (100 #x878700)
    (101 #x87875f)
    (102 #x878787)
    (103 #x8787af)
    (104 #x8787d7)
    (105 #x8787ff)
    (106 #x87af00)
    (107 #x87af5f)
    (108 #x87af87)
    (109 #x87afaf)
    (110 #x87afd7)
    (111 #x87afff)
    (112 #x87d700)
    (113 #x87d75f)
    (114 #x87d787)
    (115 #x87d7af)
    (116 #x87d7d7)
    (117 #x87d7ff)
    (118 #x87ff00)			; chartreuse
    (119 #x87ff5f)
    (120 #x87ff87)			; LightGreen87, LightGreen3
    (121 #x87ffaf)
    (122 #x87ffd7)			; aquamarine
    (123 #x87ffff)
    (124 #xaf0000)
    (125 #xaf005f)
    (126 #xaf0087)
    (127 #xaf00af)
    (128 #xaf00d7)
    (129 #xaf00ff)
    (130 #xaf5f00)
    (131 #xaf5f5f)
    (132 #xaf5f87)
    (133 #xaf5faf)
    (134 #xaf5fd7)
    (135 #xaf5fff)
    (136 #xaf8700)
    (137 #xaf875f)
    (138 #xaf8787)			; rosy brown
    (139 #xaf87af)
    (140 #xaf87d7)
    (141 #xaf87ff)			; medium purple
    (142 #xafaf00)
    (143 #xafaf5f)
    (144 #xafaf87)
    (145 #xafafaf)			; dark grey
    (146 #xafafd7)
    (147 #xafafff)
    (148 #xafd700)
    (149 #xafd75f)
    (150 #xafd787)
    (151 #xafd7af)
    (152 #xafd7d7)
    (153 #xafd7ff)
    (154 #xafff00)
    (155 #xafff5f)
    (156 #xafff87)
    (157 #xafffaf)			; LightGreenAF, LightGreen2
    (158 #xafffd7)
    (159 #xafffff)
    (160 #xd70000)
    (161 #xd7005f)
    (162 #xd70087)
    (163 #xd700af)
    (164 #xd700d7)
    (165 #xd700ff)
    (166 #xd75f00)
    (167 #xd75f5f)			; indian red
    (168 #xd75f87)
    (169 #xd75faf)
    (170 #xd75fd7)
    (171 #xd75fff)
    (172 #xd78700)
    (173 #xd7875f)
    (174 #xd78787)
    (175 #xd787af)
    (176 #xd787d7)
    (177 #xd787ff)
    (178 #xd7af00)
    (179 #xd7af5f)
    (180 #xd7af87)			; tan
    (181 #xd7afaf)
    (182 #xd7afd7)
    (183 #xd7afff)
    (184 #xd7d700)
    (185 #xd7d75f)
    (186 #xd7d787)
    (187 #xd7d7af)
    (188 #xd7d7d7)			; light grey
    (189 #xd7d7ff)
    (190 #xd7ff00)
    (191 #xd7ff5f)
    (192 #xd7ff87)
    (193 #xd7ffaf)
    (194 #xd7ffd7)			; LightGreenD7, LightGreen1
    (195 #xd7ffff)			; light cyan
    (196 #xff0000)			; red
    (197 #xff005f)
    (198 #xff0087)
    (199 #xff00af)
    (200 #xff00d7)
    (201 #xff00ff)			; magenta
    (202 #xff5f00)
    (203 #xff5f5f)
    (204 #xff5f87)
    (205 #xff5faf)			; hot pink
    (206 #xff5fd7)
    (207 #xff5fff)
    (208 #xff8700)			; dark orange
    (209 #xff875f)
    (210 #xff8787)
    (211 #xff87af)
    (212 #xff87d7)
    (213 #xff87ff)
    (214 #xffaf00)
    (215 #xffaf5f)
    (216 #xffaf87)
    (217 #xffafaf)
    (218 #xffafd7)
    (219 #xffafff)
    (220 #xffd700)			; gold
    (221 #xffd75f)
    (222 #xffd787)			; navajo white
    (223 #xffd7af)			; peach puff
    (224 #xffd7d7)
    (225 #xffd7ff)
    (226 #xffff00)			; yellow
    (227 #xffff5f)
    (228 #xffff87)
    (229 #xffffaf)
    (230 #xffffd7)			; light goldenrod yello
    (231 #xffffff)			; White

    (232 #x080808)
    (233 #x121212)
    (234 #x1c1c1c)
    (235 #x262626)
    (236 #x303030)
    (237 #x3a3a3a)
    (238 #x444444)
    (239 #x4e4e4e)
    (240 #x585858)
    (241 #x626262)
    (242 #x6c6c6c)			; dim gray
    (243 #x767676)
    (244 #x808080)
    (245 #x8a8a8a)
    (246 #x949494)
    (247 #x9e9e9e)
    (248 #xa8a8a8)			; dark grey
    (249 #xb2b2b2)			; grey
    (250 #xbcbcbc)
    (251 #xc6c6c6)
    (252 #xd0d0d0)			; light grey
    (253 #xdadada)
    (254 #xe4e4e4)
    (255 #xeeeeee)
    )
  )

(define PIP_GREEN_INDEX_COLOR_INDICES
  (list COLOR_INDEX_BLACK		; pip green index 0
	COLOR_INDEX_PIPGREEN5		; 1
	COLOR_INDEX_PIPGREEN4		; 2
	COLOR_INDEX_PIPGREEN3		; 3
	COLOR_INDEX_PIPGREEN2		; 4
	COLOR_INDEX_PIPGREEN1		; 5
	COLOR_INDEX_PIPLIGHTGREEN4	; 6
	COLOR_INDEX_PIPLIGHTGREEN3	; 7
	COLOR_INDEX_PIPLIGHTGREEN2	; 8
	COLOR_INDEX_PIPLIGHTGREEN1	; pip green index 9
	COLOR_INDEX_WHITE))		; pip green index 10

(define (pip-green-level->color-index c)
  "Converts a pip green level (zero to ten)
to an xterm color number"
  (list-ref PIP_GREEN_INDEX_COLOR_INDICES c))

(define (rgb-to-brightness RGB)
  "Converts an RGB colorref #xRRGGBB into a brighness from 0.0 to 1.0"
  (let ((D (/ 1.0 255.0)))
     (let ((R (* D (ash (logand #xFF0000 RGB) -16)))
	   (G (* D (ash (logand #x00FF00 RGB) -8)))
	   (B (* D (logand #x0000FF RGB))))
       (+ (* R 0.3)
	  (* G 0.6)
	  (* B 0.1)))))

(define (color-index-get-brightness idx)
  "Returns an intensity between 0.0 (black) and 1.0 (white)"
  (rgb-to-brightness (cadr (list-ref *xterm-colors* idx))))


(define (pip-green-level-get-brightness i)
  "Converts a pip green level to a brightness value between 0.0 and
1.0"
  (rgb-to-brightness (cadr (list-ref *xterm-colors* (pip-green-level->color-index i)))))

(define (pip-green-level-from-brightness B)
  "Converts a brightness level between 0.0 and 1.0 to a pip green
level from 0 to 10"
  (let loop ((i 0))
    (if (>= B (pip-green-level-get-brightness i))
	(loop (1+ i))
	i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLOR PAIR MANAGER


;; An alist
;; Key is 256 * bg-color-index + fg-color-index
;; Value is an ncurses color-pair index.
;; Initializes new pair as necessary.
(define *color-pair-alist* '())
(define *next-avail-color-pair* 1)

(define (color-indices-get-color-pair-index fg-color-index bg-color-index)
  "Given foreground and background xterm color indices, it returns
the color pair index associated with this pair."
  (let ((key (+ fg-color-index (* 256 bg-color-index))))
    (or (assq-ref *color-pair-alist* key)
	(begin
	  (set! *color-pair-alist* (assq-set! *color-pair-alist* key *next-avail-color-pair*))
	  (init-pair! *next-avail-color-pair* fg-color-index bg-color-index)
	  (set! *next-avail-color-pair* (1+ *next-avail-color-pair*))
	  (1- *next-avail-color-pair*)))))

(define (pip-green-levels-get-color-pair-index fg bg)
  "Given foreground and background pip green levels, it returns
the color pair index associated with this pair of green levels."
  (color-indices-get-color-pair-index
   (pip-green-level->color-index fg)
   (pip-green-level->color-index bg)))


