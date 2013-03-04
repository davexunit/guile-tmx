(define-module (tmx)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:use-module (ice-9 format)
  #:use-module (zlib)
  #:use-module (tmx base64)
  #:export (load-tmx-map
            make-tmx-map
            tmx-map?
            tmx-map-version
            tmx-map-orientation
            tmx-map-width
            tmx-map-height
            tmx-map-tile-width
            tmx-map-tile-height
            tmx-map-tilesets
            tmx-map-layers
            make-tmx-layer
            tmx-layer?
            tmx-layer-name
            tmx-layer-width
            tmx-layer-height
            tmx-layer-tiles
            make-tmx-image
            tmx-image?
            tmx-image-source
            tmx-image-width
            tmx-image-height
            make-tmx-tile
            tmx-tile?
            tmx-tile-id
            tmx-tile-properties
            make-tmx-tileset
            tmx-tileset?
            tmx-tileset-name
            tmx-tileset-first-gid
            tmx-tileset-tile-width
            tmx-tileset-tile-height
            tmx-tileset-image
            tmx-tileset-tiles))

;; Data structures
(define-record-type <tmx-map>
  (make-tmx-map version orientation width height tile-width tile-height
                tilesets layers properties)
  tmx-map?
  (version     tmx-map-version)
  (orientation tmx-map-orientation)
  (width       tmx-map-width)
  (height      tmx-map-height)
  (tile-width  tmx-map-tile-width)
  (tile-height tmx-map-tile-height)
  (tilesets    tmx-map-tilesets)
  (layers      tmx-map-layers)
  (properties  tmx-map-properties))

(define-record-type <tmx-layer>
  (make-tmx-layer name width height tiles properties)
  tmx-layer?
  (name        tmx-layer-name)
  (width       tmx-layer-width)
  (height      tmx-layer-height)
  (tiles       tmx-layer-tiles)
  (properties  tmx-map-properties))

(define-record-type <tmx-image>
  (make-tmx-image source width height)
  tmx-image?
  (source      tmx-image-source)
  (width       tmx-image-width)
  (height      tmx-image-height))

(define-record-type <tmx-tile>
  (make-tmx-tile id properties)
  tmx-tile?
  (id         tmx-tile-id)
  (properties tmx-tile-properties))

(define-record-type <tmx-tileset>
  (make-tmx-tileset name first-gid tile-width tile-height image
                    tiles properties)
  tmx-tileset?
  (name        tmx-tileset-name)
  (first-gid   tmx-tileset-first-gid)
  (tile-width  tmx-tileset-tile-width)
  (tile-height tmx-tileset-tile-height)
  (image       tmx-tileset-image)
  (tiles       tmx-tileset-tiles)
  (properties  tmx-map-properties))

;; Custom printer so we don't print out giant tile vectors.
(set-record-type-printer!
 <tmx-layer>
 (lambda (layer port)
   (format port "#<<tmx-layer> name: ~A width: ~d height: ~d>"
           (tmx-layer-name layer)
           (tmx-layer-width layer)
           (tmx-layer-height layer))))

;; Helper functions for sxml
(define (get-attr attr node)
  (car ((sxpath `(@ ,attr *text*)) node)))

(define (get-text node)
  (car ((sxpath '(*text*)) node)))

(define (load-tmx-properties tree)
  (let ((properties (make-hash-table)))
    (for-each (lambda (p)
                (let ((name  (get-attr 'name p))
                      (value (get-attr 'value p)))
                  (hash-set! properties (string->symbol name) value)))
              ((sxpath '(property)) tree))
    properties))

(define (uncompress-layer data compression)
  (cond ((string=? compression "zlib")
         (uncompress data))
        (else
         (throw 'tmx-unsupported-compression compression))))

(define (decode-layer data encoding)
  (cond ((string=? encoding "base64")
         (base64-decode (string-trim-both data)))
        (else
         (throw 'tmx-unsupported-encoding encoding))))

(define (parse-tmx-layer-data data)
  (let* ((encoding    (get-attr 'encoding data))
         (compression (get-attr 'compression data))
         (text        (get-text data)))
    (uncompress-layer (decode-layer text encoding) compression)))

(define (load-tmx-layer layer)
  (let* ((width  (string->number (get-attr 'width layer)))
         (height (string->number (get-attr 'height layer)))
         (name   (get-attr 'name layer))
         (tiles  (parse-tmx-layer-data (car ((sxpath '(data)) layer)))))
    (make-tmx-layer name width height tiles (load-tmx-properties layer))))

(define (load-tmx-layers tree)
  (let ((layers ((sxpath '(layer)) tree)))
    (map (lambda (l) (load-tmx-layer l)) layers)))

(define (load-tmx-image image)
  (let* ((source (get-attr 'source image))
         (width  (string->number (get-attr 'width image)))
         (height (string->number (get-attr 'height image))))
    (make-tmx-image source width height)))

(define (load-tmx-tile tile)
  (let ((id (get-attr 'id tile)))
    (make-tmx-tile id (load-tmx-properties tile))))

(define (load-tmx-tiles tiles)
  (map (lambda (t) (load-tmx-tile t)) tiles))

(define (load-tmx-tileset tileset)
  (let* ((name        (get-attr 'name tileset))
         (first-gid   (string->number (get-attr 'firstgid tileset)))
         (tile-width  (string->number (get-attr 'tilewidth tileset)))
         (tile-height (string->number (get-attr 'tileheight tileset)))
         (image       (car ((sxpath '((image 1))) tileset)))
         (tiles       ((sxpath '(tile)) tileset)))
    (make-tmx-tileset name first-gid tile-width tile-height
                      (load-tmx-image image) (load-tmx-tiles tiles)
                      (load-tmx-properties tileset))))

(define (load-tmx-tilesets tree)
  (let ((tilesets ((sxpath '(tileset)) tree)))
    (map (lambda (t) (load-tmx-tileset t)) tilesets)))

(define (load-tmx-map filename)
  "Parses .tmx file and returns a <tmx-map> object containing the parsed data."
  (call-with-input-file filename
    (lambda (port)
      (let* ((map         (car ((sxpath '(// map)) (xml->sxml port))))
             (version     (get-attr 'version map))
             (orientation (get-attr 'orientation map))
             (width       (string->number (get-attr 'width map)))
             (height      (string->number (get-attr 'height map)))
             (tile-width  (string->number (get-attr 'tilewidth map)))
             (tile-height (string->number (get-attr 'tileheight map)))
             (tilesets    (load-tmx-tilesets map))
             (layers      (load-tmx-layers map)))
        (make-tmx-map version orientation width height
                      tile-width tile-height tilesets layers
                      (load-tmx-properties map))))))
