;; Netscape elements and attributes
;; An html-helper-mode module

;; Marc Hedlund <hedlund@best.com> - April Fools' Day, 1995
;; version: 1.1

;; This code is distributed under the same terms as html-helper-mode
;; itself.  html-helper-mode for Emacs is written and maintained by
;; Nelson Minar <nelson@santafe.edu>, and is available from
;; <URL:http://www.santafe.edu/~nelson/tools/>.  Nelson takes no
;; responsibility for what I have done with his software!

;; The extensions to HTML supported by this module are documented at
;; <URL:http://home.netscape.com/assist/net_sites/html_extensions.html>.

;; This code must be called *after* html-helper-mode loads.  It can be
;; run nicely from the html-helper-load-hook.

;;;;

;; First, define some attributes used in Netscape tags.  These all insert a 
;; space followed by the attribute name.  This is awkward, but it's somewhat
;; better than having one keystroke for each permutation.  (Some tags, like
;; <img>, take a ton of different attributes!)

;; Tell html-helper-mode what we're up to:
(html-helper-add-type-to-alist
 '(nsattrib . (netscape-attributes-map "\C-c\C-z" netscape-attributes-menu 
  "netscape attributes")))

(html-helper-install-type 'nsattrib)

;; Then define the cookies (reversed in menu listing):

(html-helper-add-cookie
 '(nsattrib "s" "" "start-attribute" (" start=\"" 
  (r . "Starting number: ") "\"")))
 
(html-helper-add-cookie
 '(nsattrib "v" "" "value-attribute" (" value=\"" (r . "Value: ") "\"")))

(html-helper-add-cookie
 '(nsattrib "t" "" "type-attribute" (" type=\"" (r . "Type: ") "\"")))
 
(html-helper-add-cookie
 '(nsattrib "b" "" "border-attribute" (" border=\"" 
  (r . "Border (pixels): ") "\"")))

(html-helper-add-cookie
 '(nsattrib "e" "" "edge-space-attributes" 
  (" hspace=\"" (r . "Horizontal space (pixels): ") 
  "\"  vspace=\"" (r . "Vertical space (pixels): ") "\"")))
 
(html-helper-add-cookie
 '(nsattrib "x" "" "dimension-attributes" (" height=\"" (r . "Height: ") 
  "\"  width=\"" (r . "Width: ") "\"")))

(html-helper-add-cookie
 '(nsattrib "w" "" "width-attribute" (" width=\"" (r . "Width: ") "\"")))

(html-helper-add-cookie
 '(nsattrib "h" "" "height-attribute" (" height=\"" (r . "Height: ") "\"")))
 
(html-helper-add-cookie
 '(nsattrib "z" "" "size-attribute" (" size=\"" (r . "Size: ") "\"")))
 
(html-helper-add-cookie
 '(nsattrib "a" "" "align-attribute" (" align=\"" (r . "Alignment: ") "\"")))
 
;;;;

;; The following are either Netscape-specific elements (tags), or
;; commonly-used tags that take special Netscape attributes and deserve
;; a single keystroke of their own.

;; Tell html-helper-mode what we're up to:
(html-helper-add-type-to-alist
 '(nselement . (netscape-elements-map "\C-c\C-n" netscape-elements-menu 
  "netscape elements")))

(html-helper-install-type 'nselement)

;; Then define the cookies (reversed in menu listing):

;; everyone's favorite
(html-helper-add-cookie
 '(nselement "!" "<blink>" "annoy-user" ("<blink>" 
  (r . "Blinking text: ") "</blink>")))
 
(html-helper-add-cookie
 '(nselement "w" "<wbr>" "word-break" ("<wbr>")))

(html-helper-add-cookie
 '(nselement "n" "<nobr>" "no-break" ("<nobr>" (r . "Text: ") "</nobr>")))
 
(html-helper-add-cookie
 '(nselement "i" "<isindex" "isindex-prompt" ("<isindex prompt=\"" 
  (r . "Prompt: ") "\">")))
 
(html-helper-add-cookie
 '(nselement "b" "<basefont" "basefont-size" ("<basefont size=\"" 
  (r . "Size: ") "\">")))
 
(html-helper-add-cookie
 '(nselement "f" "<font" "font-size" ("<font size=\"" (r . "Size: ") "\">" 
  (r . "Text: ") "</font>")))
 
(html-helper-add-cookie
 '(nselement "c" "<center>" "center" ("<center>" (r . "Text: ") "</center>")))

;; The next three cookies are not Netscape-specific, but have Netscape-specific
;; attributes and are commonly used.  I've assigned them a keystroke based on
;; the equivalent tag in html-helper-mode proper.
(html-helper-add-cookie
 '(nselement "e" "<img" "img-with-plenty" ("<img src=\"" 
  (r . "Image source: ") "\" alt=\"" (r . "Alternative text: ") "\" align=\""
  (r . "Alignment: ") "\" height=\"" (r . "Height (pixels): ") "\" width=\""
  (r . "Width (pixels): ") "\">")))

(html-helper-add-cookie
 '(nselement "=" "<hr" "sized-rule" ("<hr width=\"" 
  (r . "Width: ") "\" align=\"" (r . "Align: ") "\">")))

(html-helper-add-cookie
 '(nselement "\C-m" "<br" "clear-break" ("<br clear=\"" 
  (r . "Clear: ") "\">"))) 

;; Finally, tell html-helper-mode to rebuild its menus including these tags.

(html-helper-rebuild-menu)
 
;; This module ends here.
