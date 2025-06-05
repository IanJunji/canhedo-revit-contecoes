(defun c:GGInserir ()
  (vl-load-com)

  ;; ========= CONFIGURAÇÃO =========
  (defun get-config-path ()
    (strcat (getenv "TEMP") "\\GGbrics_config.txt")
  )

  (defun carregar-config ()
    (if (findfile (get-config-path))
      (progn
        (setq arq (open (get-config-path) "r"))
        (setq escalaLida (read-line arq))
        (setq estacaLida (read-line arq))
        (setq eixoLido (read-line arq))
        (setq cotaLida (read-line arq))
        (close arq)
        (list escalaLida estacaLida eixoLido cotaLida)
      )
      ;; Valores padrão
      (list "esc10" "0" "100" "800")
    )
  )

  (defun salvar-config (escala estaca eixo cota)
    (setq arq (open (get-config-path) "w"))
    (write-line escala arq)
    (write-line estaca arq)
    (write-line eixo arq)
    (write-line cota arq)
    (close arq)
  )

  ;; ========= CRIAÇÃO DO DCL =========
  (defun gerar-dcl ()
    (setq dclPath (strcat (getenv "TEMP") "\\GGinserir_temp.dcl"))
    (setq dcl_id (open dclPath "w"))
    (write-line 
      "gginserir : dialog {
        label = \"PALITOS - CONFIGURAÇÕES DE INSERÇÃO\";

        : boxed_radio_column {
          label = \"EXAGERO VERTICAL\";
          key = \"escala\";
          : radio_button { label = \"NENHUM\"; key = \"esc1\"; }
          : radio_button { label = \"10 VEZES\"; key = \"esc10\"; }
        }

        : edit_box { label = \"ESTACA INICIAL\"; key = \"estaca\"; edit_width = 10; }
        : edit_box { label = \"DISTÂNCIA MÁXIMA EIXO-PALITO\"; key = \"eixo\"; edit_width = 10; }
        : edit_box { label = \"COTA INICIAL DO PERFIL\"; key = \"cota-perfil\"; edit_width = 10; }

        : row {
          : button { label = \"OK\"; is_default = true; key = \"accept\"; }
          : button { label = \"Cancelar\"; is_cancel = true; key = \"cancel\"; }
        }
      }" dcl_id)
    (close dcl_id)
    dclPath
  )

  ;; ======= CARREGA CONFIGURACAO ANTERIOR =======
  (setq config (carregar-config))
  (setq escala-salva (nth 0 config))
  (setq estaca-inicial (nth 1 config))
  (setq eixo-inicial (nth 2 config))
  (setq cota-inicial (nth 3 config))

  ;; ======= CRIA E EXIBE DCL =======
  (setq dclPath (gerar-dcl))
  (setq dcl_id (load_dialog dclPath))
  (if (not (new_dialog "gginserir" dcl_id))
    (progn (princ "\nFalha ao abrir o diálogo.") (princ)))

  (set_tile "escala" escala-salva)
  (set_tile "estaca" estaca-inicial)
  (set_tile "eixo" eixo-inicial)
  (set_tile "cota-perfil" cota-inicial)

  (action_tile "accept"
    "(progn
       (setq escala-str (get_tile \"escala\"))
       (setq escala (if (= escala-str \"esc1\") 1 10))
       (setq estaca-escolhida (get_tile \"estaca\"))
       (setq eixo-escolhido (get_tile \"eixo\"))
       (setq cota_perfil (get_tile \"cota-perfil\"))

       ;; salvar no arquivo
       (salvar-config escala-str estaca-escolhida eixo-escolhido cota_perfil)

       ;; converter para número
       (setq estaca-escolhida (atoi estaca-escolhida))
       (setq eixo-escolhido (atoi eixo-escolhido))
       (setq cota_perfil (atoi cota_perfil))

       (done_dialog 1)
     )")

  (action_tile "cancel" "(done_dialog 0)")

  (setq resultado (start_dialog))
  (unload_dialog dcl_id)
  (vl-file-delete dclPath)

  (if (/= resultado 1)
    (progn (princ "\nComando cancelado.") (princ)))

  ;; ======= CONTINUAÇÃO DA ROTINA DE INSERÇÃO =======
  (setq ucs-original (getvar "UCSNAME"))
  (command "_.UCS" "VIEW")
  (setq lista-pontos-coord '())
  (setq cont_coord 0)

  (setq sistema-atual (getvar "INSUNITS"))
  (setvar "INSUNITS" 4)

  (setq polilinha (car (entsel "\nSelecione a polilinha: ")))
  (setq blocoSel (ssget '((0 . "INSERT"))))
  (setq raio 2.0)

  (setvar "OSMODE" 16383)
  (setq ponto_perfil (getpoint "\nSELECIONE O PONTO INICIAL DO PERFIL: "))
  (setvar "OSMODE" 0)

  (setq x_perfil (car ponto_perfil))
  (setq y_perfil (cadr ponto_perfil))

  (defun dist_curva (obj pt)
    (if (and obj pt)
      (progn
        (setq param (vlax-curve-getparamatpoint obj pt))
        (vlax-curve-getdistatparam obj param)
      )
    )
  )

  (defun calcular-estaca (distancia estacaInicial nomePonto)
  (setq numEstacas (fix (/ distancia 20)))
  (setq metrosAdicionais (- distancia (* numEstacas 20)))
  (setq metrosAdicionais (fix metrosAdicionais)) ; <-- aqui ocorre o truncamento
  (setq estacaFinal (+ estacaInicial numEstacas))
  (setq metrosAdicionaisFormatados 
        (if (< metrosAdicionais 10)
          (strcat "0" (itoa metrosAdicionais))
          (itoa metrosAdicionais)))
  (setq estacaFormatada (strcat (itoa estacaFinal) "+" metrosAdicionaisFormatados))

  (setq par_coord (nth cont_coord lista-pontos-coord))
  (setq cont_coord (+ cont_coord 1))

  (setq norteStr_final (car par_coord))
  (setq lesteStr_final (cadr par_coord))

  (setq lista-estacas (append lista-estacas (list (list nomePonto norteStr_final lesteStr_final estacaFormatada))))
)

  (if (and polilinha blocoSel)
    (progn
      (setq n (sslength blocoSel)
            i 0
            circles nil
            lista-estacas nil)

      (while (< i n)
        (setq bloco (ssname blocoSel i))
        (setq ptInsercao (cdr (assoc 10 (entget bloco))))
        (setq norteStr (strcat "N=" (rtos (cadr ptInsercao) 2 4)))
        (setq lesteStr (strcat "E=" (rtos (car ptInsercao) 2 4)))
        (setq lista-pontos-coord (cons (list norteStr lesteStr) lista-pontos-coord))
        (setq ptProj (vlax-curve-getClosestPointTo polilinha ptInsercao))
        (setq attData (entnext bloco)
              tagValue nil
              cotaValue nil
              cor nil)

        (while (and attData (or (not tagValue) (not cotaValue)))
          (setq attData (entget attData))
          (if (and attData (eq (cdr (assoc 0 attData)) "ATTRIB"))
            (progn
              (cond
                ((and (or (wcmatch (cdr (assoc 2 attData)) "SP-*/*")
                          (wcmatch (cdr (assoc 2 attData)) "ST-*/*")
                          (wcmatch (cdr (assoc 2 attData)) "PI-*/*")
                          (wcmatch (cdr (assoc 2 attData)) "SM-*/*"))
                      (not tagValue))
                 (setq tagValue (cdr (assoc 1 attData)))
                 (cond
                   ((wcmatch (cdr (assoc 2 attData)) "SP-*/*") (setq cor 4))
                   ((wcmatch (cdr (assoc 2 attData)) "SM-*/*") (setq cor 2))
                   ((wcmatch (cdr (assoc 2 attData)) "ST-*/*") (setq cor 3))
                   ((wcmatch (cdr (assoc 2 attData)) "PI-*/*") (setq cor 1))
                 )
                )
                ((and (wcmatch (cdr (assoc 2 attData)) "COTA=*") (not cotaValue))
                 (setq cotaStr (cdr (assoc 1 attData)))
                 (setq cotaValue (atof (vl-string-subst "" "COTA=" cotaStr)))
                )
              )
            )
          )
          (setq attData (entnext (cdr (assoc -1 attData))))
        )

        (if (and tagValue cotaValue)
          (progn
            (setq oldLayer (getvar "CLAYER"))
            (setvar "CLAYER" "0")
            ;(command "_.COLOR" cor)
            ;(command "_.CIRCLE" "_NON" ptProj raio)
            ;(command "_.TEXT" "_NON" ptProj 2.5 0.0 tagValue)
            (setq circles (cons (list ptInsercao ptProj cor tagValue cotaValue norteStr lesteStr) circles))
            (setvar "CLAYER" oldLayer)
          )
        )
        (setq i (1+ i))
      )

      (setq comprimentoTotal (vlax-curve-getDistAtParam polilinha (vlax-curve-getEndParam polilinha)))
      (setq pt1 (vlax-curve-getPointAtParam polilinha (vlax-curve-getStartParam polilinha)))
      (setq xA (car pt1)
            xB (car ponto_perfil)
            distanciaX (abs (- xB xA)))

      (if (> xB xA)
        (setq distanciaX (* distanciaX 1))
        (setq distanciaX (* distanciaX -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
(foreach circle circles
  (setq ptInsercao (car circle)) ; ponto original
  (setq ptInsercao (list (car ptInsercao) (cadr ptInsercao) 0.0)) ; zera o Z
  
  (setq ptProjBruta (cadr circle))    ; projeção
  (setq distEP (distance ptInsercao ptProjBruta)) ; distância real

  (if (<= distEP eixo-escolhido)
    (progn
      (setq obj (vlax-ename->vla-object polilinha))
      (setq distPolilinha_corrigida (dist_curva obj ptProjBruta))
      (setq novoX_corrigido (car (polar (vlax-curve-getPointAtParam obj (vlax-curve-getStartParam obj)) 0.0 distPolilinha_corrigida)))
      (setq elemento_cota (* escala (nth 4 circle))) ; cotaValue
      (setq diferenca_cota (- (* cota_perfil escala) y_perfil))
      (setq elemento_cota (- elemento_cota diferenca_cota))
      (setq elemento_sond (nth 3 circle)) ; tagValue
      (setq ptProj (list (+ novoX_corrigido distanciaX) elemento_cota (caddr ptInsercao)))

      (calcular-estaca distPolilinha_corrigida estaca-escolhida elemento_sond)

      (command "_.COLOR" (nth 2 circle)) ; cor
      (command "_.CIRCLE" "_NON" ptProj raio)
      (command "_.TEXT" "_NON" ptProj 2.5 0.0 elemento_sond)

      (if (and elemento_sond (tblsearch "BLOCK" elemento_sond))
        (command "-insert" elemento_sond ptProj "1" "1" "0")
        (prompt (strcat "\nBloco " elemento_sond " não encontrado.")))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
    (prompt "\nSelecione uma polilinha e blocos corretamente.")
  )

  ;; ======== EXPORTAÇÃO DO CSV ========
  (defun exportar-csv (dados nome-arquivo)
    (setq pasta (getvar "DWGPREFIX"))
    (setq caminho (strcat pasta nome-arquivo))
    (setq arquivo (open caminho "w"))
    (if arquivo
      (progn
        (write-line "SONDAGEM,NORTE,LESTE,ESTACA" arquivo)
        (foreach item dados
          (write-line (strcat (car item) "," (cadr item) "," (caddr item) "," (cadddr item)) arquivo)
        )
        (close arquivo)
        (princ (strcat "\nArquivo CSV criado com sucesso em: " caminho))
      )
      (prompt "\nErro ao criar o arquivo CSV.")
    )
  )

  ;; Gera o CSV ao final
  (exportar-csv lista-estacas "lista-estacas.csv")

  ;; Restaura configurações
  (setvar "INSUNITS" sistema-atual)
  (if ucs-original
    (command "_.UCS" ucs-original)
    (command "_.UCS" "WORLD"))

  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:GGresetconfig ()
  (vl-load-com)
  (setq configPath (strcat (getenv "TEMP") "\\GGbrics_config.txt"))
  (if (findfile configPath)
    (progn
      (vl-file-delete configPath)
      (princ "\nConfiguração GGbrics removida com sucesso.")
    )
    (princ "\nNenhuma configuração GGbrics foi encontrada.")
  )
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:GGlinha (/ get-length ref-ent ref-data ref-type dist selset i ent entdata enttype start end mid dir mag unit-dir half-dir p1 p2 vertices)

  ;; Função para obter comprimento de LINE ou LWPOLYLINE
  (defun get-length (e)
    (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))
  )

  ;; Selecionar linha ou polilinha de referência
  (prompt "\nSelecione a linha ou polilinha de referência para definir o comprimento: ")
  (setq ref-ent (car (entsel)))
  (if (and ref-ent
           (setq ref-data (entget ref-ent))
           (member (cdr (assoc 0 ref-data)) '("LINE" "LWPOLYLINE")))
    (progn
      (setq dist (get-length (vlax-ename->vla-object ref-ent)))
      (prompt (strcat "\nComprimento de referência: " (rtos dist 2 2)))

      ;; Selecionar múltiplas linhas/polilinhas para gerar novas
      (setq selset (ssget '((0 . "LINE,LWPOLYLINE"))))
      (if selset
        (progn
          (setq i 0)
          (while (< i (sslength selset))
            (setq ent (ssname selset i))
            (setq entdata (entget ent))
            (setq enttype (cdr (assoc 0 entdata)))

            ;; Obter pontos inicial e final
            (cond
              ((= enttype "LINE")
               (setq start (cdr (assoc 10 entdata)))
               (setq end (cdr (assoc 11 entdata)))
              )
              ((= enttype "LWPOLYLINE")
               (setq vertices (vl-remove-if-not '(lambda (x) (= (car x) 10)) entdata))
               (setq start (cdr (car vertices)))
               (setq end (cdr (last vertices)))
              )
            )

            ;; Calcular ponto médio e direção
            (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) start end))
            (setq dir (mapcar '- end start))
            (setq mag (distance '(0 0 0) dir))

            (if (> mag 0)
              (progn
                ;; Direção unitária e deslocamento proporcional ao comprimento de referência
                (setq unit-dir (mapcar '(lambda (x) (/ x mag)) dir))
                (setq half-dir (mapcar '(lambda (x) (* x (/ dist 2.0))) unit-dir))

                ;; Pontos da nova polilinha
                (setq p1 (mapcar '- mid half-dir))
                (setq p2 (mapcar '+ mid half-dir))

                ;; Criar nova polilinha
                (entmakex
                  (list
                    '(0 . "LWPOLYLINE")
                    '(100 . "AcDbEntity")
                    '(100 . "AcDbPolyline")
                    (cons 90 2)
                    (cons 10 p1)
                    (cons 10 p2)
                    (cons 70 0)
                  )
                )
              )
              (prompt "\nEntidade com direção nula ignorada.")
            )
            (setq i (1+ i))
          )
        )
        (prompt "\nNenhuma entidade selecionada para aplicar a transformação.")
      )
    )
    (prompt "\nEntidade de referência inválida. Selecione uma LINE ou LWPOLYLINE.")
  )
  (princ)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


