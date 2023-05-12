;; Dec 13/2022
;; ===========================================================
;;
;; Contains functions used for embedding and retrieving knowledge.
;;

(defun precompute-knowledge-embeddings (knowledge path)
;````````````````````````````````````````````````````````````
; Precomputes embeddings for a given list of knowledge strings,
; and dumps the knowledge+embeddings into a CSV file for future use.
;
  (let (knowledge-str)
    (setq knowledge-str (mapcar #'expr-to-str knowledge))
    (information-retrieval:embed-documents knowledge-str
      :filename (concatenate 'string path "knowledge.csv")
      :api-key (get-api-key "huggingface"))
)) ; END precompute-knowledge-embeddings



(defun precompute-schema-embeddings (schemas path)
;``````````````````````````````````````````````````
; Precomputes embeddings for all stored general schemas (both headers
; and contents) and dumps to a CSV file for future use.
;
  (let (headers headers-str predicates-str facts facts-str)
    ; Embed schema headers
    (setq headers (mapcar (lambda (schema) (car (schema-header schema))) schemas))
    (setq headers-str (mapcar #'expr-to-str headers))
    (setq predicates-str (mapcar #'string (mapcar #'schema-predicate schemas)))
    (information-retrieval:embed-documents headers-str
      :filename (concatenate 'string path "schema-headers.csv")
      :api-key (get-api-key "huggingface")
      :indices predicates-str)
    
    ; Embed schema contents
    (dolist (schema schemas)
      (setq facts (get-schema-sections-wffs schema
        '(:types :rigid-conds :static-conds :preconds :postconds :goals :episodes)))
      (setq facts-str (mapcar #'expr-to-str facts))
      (information-retrieval:embed-documents facts-str
        :filename (concatenate 'string path "schemas/" (string (schema-predicate schema)) ".csv")
        :api-key (get-api-key "huggingface")))
)) ; END precompute-schema-embeddings



(defun retrieve-relevant-knowledge-from-kb (text path &optional (n 5))
;``````````````````````````````````````````````````````````````````````
; Retrieves the n most relevant facts to an input text from the
; knowledge base (assuming that the knowledge base has been
; previously embedded and stored in a CSV file).
;
  (coerce
    (information-retrieval:retrieve text
      :filename (concatenate 'string path "knowledge.csv")
      :api-key (get-api-key "huggingface")
      :n n)
    'list)
) ; END retrieve-relevant-knowledge-from-kb



(defun retrieve-relevant-epi-schemas (text path &optional (n 1))
;````````````````````````````````````````````````````````````````
; Retrieves the n most relevant epi-schemas to an input text from
; the set of general schemas, based on their schema headers (assuming
; that the schemas have been previously embedded and stored in a CSV file).
;
  (let (schema-predicates)
    (setq schema-predicates (mapcar #'intern (coerce
      (information-retrieval:retrieve text
        :filename (concatenate 'string path "schema-headers.csv")
        :api-key (get-api-key "huggingface")
        :indices t
        :n n)
      'list)))
    (mapcar #'get-stored-schema schema-predicates)
)) ; END retrieve-relevant-epi-schemas



(defun retrieve-relevant-schema-facts (text schema path &optional (n 3))
;`````````````````````````````````````````````````````````````````````````
; Retrieves the n most relevant facts contained within a given schema
; to an input text (assuming that the schema contents have been previously
; embedded and stored in a CSV file).
;
  (coerce
    (information-retrieval:retrieve text
      :filename (concatenate 'string path "schemas/" (string (schema-predicate schema)) ".csv")
      :api-key (get-api-key "huggingface")
      :n n)
    'list)
) ; END retrieve-relevant-schema-facts