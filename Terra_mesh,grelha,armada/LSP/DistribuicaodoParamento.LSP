;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;      IAN LISP Utilities - Ferramentas de Geometria para AutoCAD        ;;;
;;;                                                                        ;;;
;;;------------------------------------------------------------------------;;;
;;;                                                                        ;;;
;;; Comandos Definidos:                                                    ;;;
;;;   GERAR_PONTOS_REGUA           (GPR)                                   ;;;
;;;     Gera um arquivo TXT com coordenadas de pontos ao longo de curvas,  ;;;
;;;     usando um método de "régua de aço" (cordas de comprimento fixo).   ;;;
;;;                                                                        ;;;
;;;   COLOCAR_BLOCO_EM_PONTOS_MEDIOS (CBPM)                                ;;;
;;;     Lê coordenadas de um arquivo TXT, calcula os pontos médios entre   ;;;
;;;     pares de pontos consecutivos e insere um bloco selecionado nesses  ;;;
;;;     pontos médios, alinhado com o segmento original.                   ;;;
;;;                                                                        ;;;
;;; -----------------------------------------------------------------------;;;
;;; Autor: ian junji                                                       ;;;
;;; Versão: 0.5                                                            ;;;
;;;; ----------------------------------------------------------------------;;;;

(vl-load-com) ; Necessário para funções VLA (Visual LISP ActiveX)

;;; --- Funções Auxiliares Reutilizáveis ---

;; Abre um arquivo de forma segura, retorna o handle ou nil se falhar
(defun IAN:safe-open (path mode / h) 
  (cond 
    ((setq h (open path mode)) h)
    (t (princ (strcat "\n[ERRO] Falha ao abrir: " path)) nil)
  )
)

;; Fecha um arquivo se o handle não for nil, retorna sempre nil
(defun IAN:close-handle (h) 
  (if h (progn (close h) nil) nil)
)

;; Manipulador de erro padrão: executa função de restauração e mostra mensagem se necessário
(defun IAN:std-error (msg restore-fn) 
  (if restore-fn 
    (progn 
      (restore-fn) ; Executa a função de restauração
    )
  )
  (if 
    (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
    (princ (strcat "\nErro: " msg))
  )
  (princ)
)

;; Desativa o OSNAP, retorna o valor anterior para restauração posterior
(defun IAN:disable-osnap (/ prev) 
  (setq prev (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  prev
)

;; Restaura o OSNAP para o valor anterior, se fornecido
(defun IAN:restore-osnap (prev) 
  (if prev 
    (progn 
      (setvar "OSMODE" prev)
    )
    ;; else (opcional): (princ "\n[IAN:restore-osnap] 'prev' é nil, nada a restaurar.")
  )
)

;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;                     COMANDOS                                           ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;

(defun c:COMANDOS (/) 
  ;; Muda para a tela de texto para garantir que a lista seja bem visualizada.
  (textscr)

  ;; Imprime o cabeçalho da lista de comandos.
  (princ "\n=================================================================")
  (princ "\n     Contenções LSP - Comandos Disponíveis")
  (princ "\n=================================================================")
  (princ "\n") ;; Linha em branco para espaçamento

  ;; Comando 1: GERAR_PONTOS_REGUA
  (princ "\n1. GERAR_PONTOS_REGUA (Alias: GPR)")
  (princ "\n   --------------------------------------------------------------")
  (princ "\n   Descrição: Gera um arquivo TXT com coordenadas de pontos ao")
  (princ "\n              longo de curvas, usando um método de cordas de comprimento fixo.")
  (princ "\n") ;; Linha em branco para espaçamento

  ;; Comando 2: COLOCAR_BLOCO_EM_PONTOS_MEDIOS
  (princ "\n2. COLOCAR_BLOCO_EM_PONTOS_MEDIOS (Alias: CBPM)")
  (princ "\n   --------------------------------------------------------------")
  (princ "\n   Descrição: Lê coordenadas de um arquivo TXT, calcula os pontos")
  (princ "\n              médios entre pares de pontos e insere um bloco")
  (princ "\n              selecionado nesses pontos, alinhado com o segmento")
  (princ "\n              original.")
  (princ "\n") ;; Linha em branco para espaçamento

  ;; Comando 3: ALTERAR ALTURA DE BLOCOS EM SEQUÊNCIA
  (princ "\n3. ALTBLOCOSEQ (Alias: ABS)") ; Ou o nome que você preferir
  (princ "\n   --------------------------------------------------------------")
  (princ "\n   Descrição: Modifica a altura Z de blocos em uma sequência")
  (princ "\n              (baseada no mesmo nome de bloco e proximidade),")
  (princ "\n              de forma intercalada (um sim, um não).")
  (princ "\n=================================================================")
  (princ "\n") ;; Linha em branco para espaçamento

  ;; Comando 4: DELETAR BLOCOS INTERCALADOS
  (princ "\n4. APAGAR_BLOCOS_INTERCALADOS_INVERSO (Alias: DELBLOCINV)")
  (princ "\n   --------------------------------------------------------------")
  (princ "\n   Descrição: Apaga blocos em uma sequência (mesmo nome, por")
  (princ "\n              proximidade), de forma intercalada (primeiro sim,")
  (princ "\n              segundo não, etc.).")
  (princ "\n=================================================================")
  (princ "\n") ;; Linha em branco para espaçamento

  ;; (princ) final para suprimir o retorno de nil na linha de comando do AutoCAD.
  (princ)
)


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;             COMANDO: GERAR_PONTOS_REGUA (GPR)                          ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;

;;; --- Função Auxiliar para GPR: Encontrar Próximo Ponto com Régua (v0.7) ---
(defun IAN:gpr-find-next-point (obj_curva pt_inicio comp_regua tol / mspace 
                                melhor_ponto_B circulo_temp_raw circulo_obj 
                                pontos_intersecao_variant pontos_intersecao_sa 
                                arr_pontos i lista_de_pontos_intersecao param_A_catch 
                                param_A menor_param_valido_a_frente pt_int 
                                param_int_catch param_int
                               ) 
  (princ 
    (strcat "\n  [GPR DEBUG] Régua de " 
            (rtos comp_regua)
            " a partir de: "
            (vl-princ-to-string pt_inicio)
    )
  )
  (setq mspace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq melhor_ponto_B nil)
  (setq circulo_obj nil)
  (setq centro_circulo_3d pt_inicio)

  (setq circulo_temp_raw (vl-catch-all-apply 'vla-AddCircle 
                                             (list mspace 
                                                   (vlax-3d-point centro_circulo_3d)
                                                   comp_regua
                                             )
                         )
  )

  (if (vl-catch-all-error-p circulo_temp_raw) 
    (princ 
      (strcat "\n  [GPR ERRO] Falha ao criar círculo temporário: " 
              (vl-catch-all-error-message circulo_temp_raw)
      )
    )
    (progn 
      (setq circulo_obj circulo_temp_raw)

      (setq pontos_intersecao_variant (vla-IntersectWith obj_curva 
                                                         circulo_obj
                                                         acExtendNone
                                      )
      )
      (vla-Delete circulo_obj)
      (setq pontos_intersecao_sa (vlax-variant-value pontos_intersecao_variant))

      (if 
        (and pontos_intersecao_sa 
             (eq (type pontos_intersecao_sa) 'SAFEARRAY)
             (= (vlax-safearray-get-dim pontos_intersecao_sa) 1)
             (> (vlax-safearray-get-u-bound pontos_intersecao_sa 1) -1)
        )
        (progn 
          (setq lista_de_pontos_intersecao nil)
          (setq arr_pontos (vlax-safearray->list pontos_intersecao_sa))
          (setq i 0)
          (while (< i (length arr_pontos)) 
            (setq lista_de_pontos_intersecao (cons 
                                               (list (nth i arr_pontos) 
                                                     (nth (1+ i) arr_pontos)
                                                     (nth (+ i 2) arr_pontos)
                                               )
                                               lista_de_pontos_intersecao
                                             )
            )
            (setq i (+ i 3))
          )
          (setq lista_de_pontos_intersecao (reverse lista_de_pontos_intersecao))
          (princ 
            (strcat "\n    [GPR DEBUG] " 
                    (itoa (length lista_de_pontos_intersecao))
                    " interseções cruas: "
                    (vl-princ-to-string lista_de_pontos_intersecao)
            )
          )

          (setq param_A_catch (vl-catch-all-apply 'vlax-curve-getParamAtPoint 
                                                  (list obj_curva pt_inicio)
                              )
          )
          (if (vl-catch-all-error-p param_A_catch) 
            (progn 
              (princ 
                (strcat "\n    [GPR ERRO] Parâmetro pt_inicio: " 
                        (vl-catch-all-error-message param_A_catch)
                )
              )
              (setq param_A nil)
            )
            (setq param_A param_A_catch)
          )
          (if param_A 
            (progn 
              (setq menor_param_valido_a_frente nil)
              (foreach pt_int lista_de_pontos_intersecao 
                (if 
                  (and (> (distance pt_int pt_inicio) tol) 
                       (equal (distance pt_int pt_inicio) comp_regua tol)
                  )
                  (progn 
                    (setq param_int_catch (vl-catch-all-apply 'vlax-curve-getParamAtPoint 
                                                              (list obj_curva 
                                                                    pt_int
                                                              )
                                          )
                    )
                    (if (not (vl-catch-all-error-p param_int_catch)) 
                      (progn 
                        (setq param_int param_int_catch)
                        (if (> param_int (+ param_A (* tol 0.01))) 
                          (if 
                            (or (null menor_param_valido_a_frente) 
                                (< param_int menor_param_valido_a_frente)
                            )
                            (progn (setq menor_param_valido_a_frente param_int) 
                                   (setq melhor_ponto_B pt_int)
                            )
                          )
                        )
                      )
                      (princ 
                        (strcat "\n    [GPR ERRO] Parâmetro pt_int " 
                                (vl-princ-to-string pt_int)
                                ": "
                                (vl-catch-all-error-message param_int_catch)
                        )
                      )
                    )
                  )
                )
              )
              (if melhor_ponto_B 
                (princ 
                  (strcat "\n    [GPR DEBUG] Ponto B selecionado: " 
                          (vl-princ-to-string melhor_ponto_B)
                  )
                )
                (princ "\n    [GPR DEBUG] Nenhum ponto B válido encontrado.")
              )
            )
          )
        )
        (princ "\n    [GPR DEBUG] Nenhuma interseção válida com círculo.")
      )
    )
  )
  melhor_ponto_B
)

;;; --- Comando Principal: GERAR_PONTOS_REGUA ---
(defun c:GERAR_PONTOS_REGUA (/ *error* ss_linhas_alvo contador ent_nome ent_obj 
                             comprimento_regua arquivo_saida lista_todos_os_pontos f 
                             ponto_A ponto_B lista_pontos_entidade_atual 
                             primeiro_ponto_adicionado tolerancia 
                             start_pt_entidade_catch old_osmode
                            ) 
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (setq f (IAN:close-handle f))
          (IAN:restore-osnap old_osmode)
        )
      )
    )
  )
  (setq old_osmode (IAN:disable-osnap))
  (setq tolerancia 0.0001)
  (setq comprimento_regua nil)
  (princ "\n>>> GERADOR DE PONTOS COM RÉGUA (GPR) <<<")
  (princ "\nSelecione as entidades alvo (linhas, polilinhas, arcos, splines): ")
  (setq ss_linhas_alvo (ssget '((0 . "LINE,LWPOLYLINE,ARC,SPLINE"))))

  (if (null ss_linhas_alvo) 
    (progn (princ "\nNenhuma entidade selecionada. Comando cancelado.") 
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )

  (while (or (null comprimento_regua) (<= comprimento_regua 0.0)) 
    (setq comprimento_regua (getdist "\nDigite o comprimento da régua/segmento: "))
    (if (or (null comprimento_regua) (<= comprimento_regua 0.0)) 
      (princ "\nComprimento inválido. Deve ser positivo.")
    )
  )
  (setq arquivo_saida (getfiled "Salvar arquivo de coordenadas TXT" "" "txt" 1))
  (if (null arquivo_saida) 
    (progn (princ "\nNenhum arquivo selecionado. Comando cancelado.") 
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )

  (setq lista_todos_os_pontos nil)
  (setq contador 0)
  (princ 
    (strcat "\nProcessando " (itoa (sslength ss_linhas_alvo)) " entidade(s)...")
  )

  (while (< contador (sslength ss_linhas_alvo)) 
    (setq ent_nome (ssname ss_linhas_alvo contador))
    (setq ent_obj (vlax-ename->vla-object ent_nome))
    (princ 
      (strcat "\nAnalisando entidade " 
              (itoa (1+ contador))
              " ("
              (vla-get-ObjectName ent_obj)
              "):"
      )
    )
    (setq lista_pontos_entidade_atual nil)
    (setq primeiro_ponto_adicionado nil)

    (setq start_pt_entidade_catch (vl-catch-all-apply 'vlax-curve-getStartPoint 
                                                      (list ent_obj)
                                  )
    )
    (if (vl-catch-all-error-p start_pt_entidade_catch) 
      (progn 
        (princ 
          (strcat "\n  [GPR AVISO] Falha ao obter ponto inicial. Pulando. (" 
                  (vl-catch-all-error-message start_pt_entidade_catch)
                  ")"
          )
        )
        (setq ponto_A nil)
      )
      (setq ponto_A start_pt_entidade_catch)
    )
    (while ponto_A 
      (setq ponto_B (IAN:gpr-find-next-point 
                      ent_obj
                      ponto_A
                      comprimento_regua
                      tolerancia
                    )
      )
      (if ponto_B 
        (progn 
          (if (not primeiro_ponto_adicionado) 
            (progn 
              (setq lista_pontos_entidade_atual (cons ponto_A 
                                                      lista_pontos_entidade_atual
                                                )
              )
              (setq primeiro_ponto_adicionado T)
            )
          )
          (setq lista_pontos_entidade_atual (cons ponto_B 
                                                  lista_pontos_entidade_atual
                                            )
          )
          (setq ponto_A ponto_B)
        )
        (setq ponto_A nil)
      )
    )
    (if lista_pontos_entidade_atual 
      (progn 
        (setq lista_pontos_entidade_atual (reverse lista_pontos_entidade_atual))
        (setq lista_todos_os_pontos (append lista_todos_os_pontos 
                                            lista_pontos_entidade_atual
                                    )
        )
        (princ 
          (strcat " -> " (itoa (length lista_pontos_entidade_atual)) " pontos.")
        )
      )
      (princ " -> Nenhum segmento completo.")
    )
    (setq contador (1+ contador))
  )
  (if lista_todos_os_pontos 
    (progn 
      (setq f (IAN:safe-open arquivo_saida "w"))
      (if f 
        (progn 
          (princ 
            (strcat "\n\nEscrevendo " 
                    (itoa (length lista_todos_os_pontos))
                    " pontos em: "
                    arquivo_saida
            )
          )
          (foreach pt lista_todos_os_pontos 
            (write-line 
              (strcat (rtos (car pt) 2 8) 
                      ","
                      (rtos (cadr pt) 2 8)
                      ","
                      (rtos (caddr pt) 2 8)
              )
              f
            )
          )
          (setq f (IAN:close-handle f))
          (princ "\nSucesso! Arquivo salvo.")
        )
        (princ 
          (strcat "\n[GPR ERRO] Falha ao abrir arquivo para escrita: " 
                  arquivo_saida
          )
        )
      )
    )
    (princ "\n\nNenhum ponto gerado para salvar.")
  )
  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:GPR () (c:GERAR_PONTOS_REGUA)) 


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;       COMANDO: COLOCAR_BLOCO_EM_PONTOS_MEDIOS (CBPM)                   ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;

;;; --- Função Auxiliar para CBPM: Converter String "X,Y,Z" para Lista de Pontos ---
(defun IAN:cbpm-string-to-pointlist (str_coords / str-split-by-comma str-to-real 
                                     coord_parts x_str y_str z_str pt_x pt_y pt_z
                                    ) 
  (defun str-split-by-comma (str / pos sub result) 
    (setq result nil)
    (setq str (vl-string-trim " " str))
    (while (setq pos (vl-string-search "," str)) 
      (setq sub (substr str 1 pos))
      (setq result (cons (vl-string-trim " " sub) result))
      (setq str (substr str (+ pos 2)))
    )
    (setq result (cons (vl-string-trim " " str) result))
    (reverse result)
  )
  (defun str-to-real (s / val) 
    (if (and s (not (equal s ""))) 
      (progn 
        (setq val (distof s))
        (if (null val) 
          (if (member (strcase s) '("0" "0.0" "0,0")) 0.0 nil)
          val
        )
      )
      nil
    )
  )
  (if (and str_coords (= (type str_coords) 'STR) (> (strlen str_coords) 0)) 
    (progn 
      (setq coord_parts (str-split-by-comma str_coords))
      (if (and coord_parts (= (length coord_parts) 3)) 
        (progn 
          (setq pt_x (str-to-real (nth 0 coord_parts)))
          (setq pt_y (str-to-real (nth 1 coord_parts)))
          (setq pt_z (str-to-real (nth 2 coord_parts)))
          (if (and pt_x pt_y pt_z) 
            (list pt_x pt_y pt_z)
            (progn 
              (princ 
                (strcat "\n  [CBPM DEBUG] Falha conversão X,Y,Z: " 
                        str_coords
                        " (X:"
                        (if pt_x "OK" (nth 0 coord_parts))
                        " Y:"
                        (if pt_y "OK" (nth 1 coord_parts))
                        " Z:"
                        (if pt_z "OK" (nth 2 coord_parts))
                        ")"
                )
              )
              nil
            )
          )
        )
        (progn 
          (princ (strcat "\n  [CBPM DEBUG] Linha não contém 3 partes: " str_coords))
          nil
        )
      )
    )
    nil
  )
)

;;; --- Função Auxiliar para CBPM: Garantir Existência da Layer ---
(defun IAN:cbpm-ensure-layer (layer_name color_index / acadDoc layersColl 
                              layerObj_raw vla_layer current_cmdecho
                             )  ; Renomeado layerObj para layerObj_raw
  (setq current_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layersColl (vla-get-Layers acadDoc))
  (setq vla_layer nil)

  (setq layerObj_raw (vl-catch-all-apply 'vla-Item (list layersColl layer_name)))

  (if (vl-catch-all-error-p layerObj_raw) 
    (progn 
      (princ (strcat "\nCriando layer: " layer_name))
      (setq layerObj_raw (vl-catch-all-apply 'vla-Add (list layersColl layer_name)))
      (if (vl-catch-all-error-p layerObj_raw) 
        (princ 
          (strcat "\n  [CBPM ERRO] Falha ao criar layer '" 
                  layer_name
                  "': "
                  (vl-catch-all-error-message layerObj_raw)
          )
        )
        (progn 
          (setq vla_layer layerObj_raw)
          (if (and color_index (vlax-method-applicable-p vla_layer 'put_Color)) 
            (vl-catch-all-apply 'vla-put-Color (list vla_layer color_index))
          )
          (if (vlax-method-applicable-p vla_layer 'put_Lock) 
            (vl-catch-all-apply 'vla-put-Lock (list vla_layer :vlax-false))
          )
          (if (vlax-method-applicable-p vla_layer 'put_Freeze) 
            (vl-catch-all-apply 'vla-put-Freeze (list vla_layer :vlax-false))
          )
          (if (vlax-method-applicable-p vla_layer 'put_Plottable) 
            (vl-catch-all-apply 'vla-put-Plottable (list vla_layer :vlax-true))
          )
          (princ (strcat " -> Cor: " (if color_index (itoa color_index) "Padrão")))
        )
      )
    )
    (setq vla_layer layerObj_raw)
  )
  (setvar "CMDECHO" current_cmdecho)
  vla_layer
)

;;; --- Comando Principal: COLOCAR_BLOCO_EM_PONTOS_MEDIOS ---
(defun c:COLOCAR_BLOCO_EM_PONTOS_MEDIOS (/ *error* arquivo_txt f linha lineno 
                                         lista_pontos_lidos p_temp i p_orig_A p_orig_B 
                                         p_medio_calc lista_segmentos_para_blocos 
                                         ent_sel_data nome_bloco_real 
                                         vla_bloco_selecionado_raw vla_bloco_modelo 
                                         layer_alvo_nome vla_layer_alvo 
                                         angulo_insercao old_clayer 
                                         contador_blocos_inseridos mspace doc 
                                         tolerancia_dist z_tolerance ; <-- Nova variável
                                         selecao_valida vla_bloco_inserido_raw 
                                         old_osmode
                                        ) 
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (setq f (IAN:close-handle f))
          (if old_clayer (setvar "CLAYER" old_clayer))
          (IAN:restore-osnap old_osmode)
        )
      )
    )
  )
  (setq old_osmode (IAN:disable-osnap))
  (setq layer_alvo_nome "BLOCOS_PONTOS_MEDIOS")
  (setq contador_blocos_inseridos 0)
  (setq lista_pontos_lidos nil)
  (setq lista_segmentos_para_blocos nil)
  (setq tolerancia_dist 0.00001)
  (setq z_tolerance 0.01) ; Tolerância para a variação em Z (ex: 1cm). Mude se necessário.
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace (vla-get-ModelSpace doc))
  (setq old_clayer (getvar "CLAYER"))

  (princ "\n>>> INSERIR BLOCOS EM PONTOS MÉDIOS (CBPM) <<<")

  (setq arquivo_txt (getfiled "Selecione o arquivo TXT com coordenadas (X,Y,Z)" 
                              ""
                              "txt"
                              0
                    )
  )
  (if (or (null arquivo_txt) (not (findfile arquivo_txt))) 
    (progn (princ "\nArquivo não selecionado/encontrado. Comando cancelado.") 
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )
  (setq f (IAN:safe-open arquivo_txt "r"))
  (if (not f) 
    (progn (princ (strcat "\n[CBPM ERRO] Falha ao abrir arquivo: " arquivo_txt)) 
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )

  (princ (strcat "\nLendo arquivo: " arquivo_txt))
  (setq lineno 0)
  (while (setq linha (read-line f)) 
    (setq lineno (1+ lineno))
    (setq p_temp (IAN:cbpm-string-to-pointlist linha))
    (if p_temp 
      (setq lista_pontos_lidos (cons p_temp lista_pontos_lidos))
      (princ 
        (strcat "\n  [CBPM AVISO] Linha " (itoa lineno) " ignorada: \"" linha "\"")
      )
    )
  )
  (setq f (IAN:close-handle f))
  (setq lista_pontos_lidos (reverse lista_pontos_lidos))

  (if (< (length lista_pontos_lidos) 2) 
    (progn (princ "\n[CBPM ERRO] Menos de 2 pontos válidos lidos. Comando cancelado.") 
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )
  (princ (strcat "\n" (itoa (length lista_pontos_lidos)) " pontos válidos lidos."))

  ;; *** LÓGICA DE PROCESSAMENTO DE SEGMENTOS MODIFICADA AQUI ***
  (setq i 0)
  (while (< i (1- (length lista_pontos_lidos))) 
    (setq p_orig_A (nth i lista_pontos_lidos))
    (setq p_orig_B (nth (1+ i) lista_pontos_lidos))

    ;; --- NOVA VERIFICAÇÃO DE Z ---
    ;; Verifica se a variação em Z está dentro da tolerância
    (if (<= (abs (- (caddr p_orig_A) (caddr p_orig_B))) z_tolerance) 
      ;; Se Z for contínuo, processa o segmento como antes
      (if (> (distance p_orig_A p_orig_B) tolerancia_dist) 
        (progn 
          (setq p_medio_calc (list (/ (+ (car p_orig_A) (car p_orig_B)) 2.0) 
                                   (/ (+ (cadr p_orig_A) (cadr p_orig_B)) 2.0)
                                   (/ (+ (caddr p_orig_A) (caddr p_orig_B)) 2.0)
                             )
          )
          (setq lista_segmentos_para_blocos (cons 
                                              (list p_medio_calc p_orig_A p_orig_B)
                                              lista_segmentos_para_blocos
                                            )
          )
        )
        (princ 
          (strcat "\n  [CBPM AVISO] Segmento " 
                  (itoa (1+ i))
                  "-"
                  (itoa (+ i 2))
                  " ignorado (curto/coincidente)."
          )
        )
      )
      ;; Se Z tiver uma quebra, ignora o segmento e avisa o usuário
      (princ 
        (strcat "\n  [CBPM AVISO] Quebra de sequência em Z detectada entre pontos " 
                (itoa (1+ i))
                " e "
                (itoa (+ i 2))
                ". Segmento ignorado."
        )
      )
    )
    (setq i (1+ i))
  )
  (setq lista_segmentos_para_blocos (reverse lista_segmentos_para_blocos))

  (if (null lista_segmentos_para_blocos) 
    (progn (princ "\n[CBPM ERRO] Nenhum segmento válido para pontos médios. Verifique o arquivo.") 
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )
  (princ 
    (strcat "\n" 
            (itoa (length lista_segmentos_para_blocos))
            " pontos médios calculados."
    )
  )

  (setq selecao_valida nil)
  (setq nome_bloco_real nil)
  (setq vla_bloco_modelo nil)
  (while (not selecao_valida) 
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo a ser inserido: "))
    (if ent_sel_data 
      (progn 
        (setq vla_bloco_selecionado_raw (vl-catch-all-apply 'vlax-ename->vla-object 
                                                            (list (car ent_sel_data))
                                        )
        )
        (if (not (vl-catch-all-error-p vla_bloco_selecionado_raw)) 
          (progn 
            (setq vla_bloco_modelo vla_bloco_selecionado_raw)
            (if 
              (and vla_bloco_modelo 
                   (vlax-property-available-p vla_bloco_modelo 'ObjectName)
                   (= (vla-get-ObjectName vla_bloco_modelo) "AcDbBlockReference")
              )
              (progn 
                (setq nome_bloco_real (vla-get-EffectiveName vla_bloco_modelo))
                (princ (strcat "\nBloco selecionado: '" nome_bloco_real "'."))
                (setq selecao_valida T)
              )
              (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
            )
          )
          (princ 
            (strcat "\n[CBPM ERRO] Conversão para objeto VLA: " 
                    (vl-catch-all-error-message vla_bloco_selecionado_raw)
            )
          )
        )
      )
      (progn (princ "\nSeleção cancelada. Comando encerrado.") 
             (IAN:restore-osnap old_osmode)
             (exit)
      )
    )
  )
  (setq vla_layer_alvo (IAN:cbpm-ensure-layer layer_alvo_nome 7))
  (if (not vla_layer_alvo) 
    (progn 
      (princ 
        (strcat "\n[CBPM AVISO] Falha layer '" 
                layer_alvo_nome
                "'. Usando layer atual: "
                old_clayer
        )
      )
      (setq layer_alvo_nome old_clayer)
    )
  )
  (princ (strcat "\nInserindo blocos na layer '" layer_alvo_nome "'..."))
  (foreach segmento lista_segmentos_para_blocos 
    (setq p_medio_calc (car segmento))
    (setq p_orig_A (cadr segmento))
    (setq p_orig_B (caddr segmento))
    (setq angulo_insercao (angle p_orig_A p_orig_B))

    (setq vla_bloco_inserido_raw (vl-catch-all-apply 'vla-InsertBlock 
                                                     (list mspace 
                                                           (vlax-3d-point p_medio_calc)
                                                           nome_bloco_real
                                                           1.0
                                                           1.0
                                                           1.0
                                                           angulo_insercao
                                                     )
                                 )
    )
    (if (not (vl-catch-all-error-p vla_bloco_inserido_raw)) 
      (progn 
        (setq contador_blocos_inseridos (1+ contador_blocos_inseridos))
        (if 
          (and vla_layer_alvo 
               (vlax-method-applicable-p vla_bloco_inserido_raw 'put_Layer)
          )
          (vla-put-Layer vla_bloco_inserido_raw layer_alvo_nome)
          (princ 
            (strcat "\n  [CBPM AVISO] Não foi possível definir a layer para o bloco inserido no ponto " 
                    (vl-princ-to-string p_medio_calc)
            )
          )
        )
        (princ ".")
      )
      (princ 
        (strcat "\n  [CBPM ERRO] Falha ao inserir bloco '" 
                nome_bloco_real
                "' em "
                (vl-princ-to-string p_medio_calc)
                ": "
                (vl-catch-all-error-message vla_bloco_inserido_raw)
        )
      )
    )
  )
  (if (> contador_blocos_inseridos 0) 
    (princ 
      (strcat "\nSucesso! " 
              (itoa contador_blocos_inseridos)
              " bloco(s) '"
              nome_bloco_real
              "' inserido(s)."
      )
    )
    (princ "\nNenhum bloco foi inserido.")
  )
  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:CBPM () (c:COLOCAR_BLOCO_EM_PONTOS_MEDIOS)) 


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;   COMANDO: IAN_RotateThenOffsetBlock (RTOB) v1.0                     ;;;
;;;   (Rotaciona no Ponto de Inserção e Desloca Perpendicularmente)        ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;
;;; Autor: ian junji (Lógica ditada, implementado por Assistente AI)       ;;;
;;; Versão: 1.0                                                            ;;;
;;; Descrição: Rotaciona blocos selecionados em 180 graus em torno de seu  ;;;
;;;            ponto de inserção (eixo Z). Em seguida, desloca cada bloco  ;;;
;;;            uma distância especificada, 90 graus em relação à nova     ;;;
;;;            orientação do bloco.                                        ;;;
;;;-----------------------------------------------------------------------------

(defun c:RotacionarGeogrelhas (/ 
                               ;; Variáveis locais
                               *error* s:old_cmdecho s:old_osmode ; Variáveis de sistema
                               offset_distance ss_blocks total_selected i ent_name 
                               vla_block current_insertion_point_vla 
                               current_insertion_point_list current_rotation_rad 
                               new_block_rotation_rad offset_angle_rad new_ins_x 
                               new_ins_y new_ins_z new_insertion_point_list 
                               processed_count error_obj
                              ) 

  ;; --- Manipulador de Erro Local Simples ---
  (defun *error* (msg) 
    (setq *error* nil) ; Previne recursão
    (princ "\n\n!!!!!!!! [RTOB v1.0 *error*] !!!!!!!!")
    (princ (strcat "\n  ERRO: " msg))
    (if s:old_cmdecho (setvar "CMDECHO" s:old_cmdecho))
    (if s:old_osmode (setvar "OSMODE" s:old_osmode))
    (princ "\n  Comando RTOB interrompido.")
    (princ "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    (princ)
  )

  ;; --- Inicialização ---
  (princ "\n>>> RTOB v1.0 - Rotacionar e Deslocar Blocos <<<")
  (vl-load-com) ; Garante que COM está carregado
  (setq s:old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq s:old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq processed_count 0)

  ;; 1. Pedir Comprimento do Offset
  (initget 1) ; Não pode ser nulo
  (setq offset_distance (getdist "\nDigite a distância para o deslocamento perpendicular (ex: 8.0): "))
  (if (null offset_distance) 
    (progn (princ "\nDistância não fornecida. Comando cancelado.") 
           (*error* "Cancelado pelo usuário")
           (exit)
    )
  )

  ;; 2. Selecionar Blocos
  (princ "\nSelecione os blocos para processar:")
  (setq ss_blocks (ssget '((0 . "INSERT"))))

  (if (null ss_blocks) 
    (progn (princ "\nNenhum bloco selecionado.") 
           (*error* "Seleção cancelada")
           (exit)
    )
  )

  (setq total_selected (sslength ss_blocks))
  (princ 
    (strcat "\n" (itoa total_selected) " bloco(s) selecionado(s). Processando...")
  )

  ;; 3. Para Cada Bloco Selecionado
  (setq i 0)
  (while (< i total_selected) 
    (setq ent_name (ssname ss_blocks i))
    (setq vla_block (vl-catch-all-apply 'vlax-ename->vla-object (list ent_name)))

    (if (vl-catch-all-error-p vla_block) 
      (princ 
        (strcat "\n  AVISO: Falha ao obter objeto VLA para " 
                (vl-princ-to-string ent_name)
                ". Erro: "
                (vl-catch-all-error-message vla_block)
        )
      )
      (progn 
        (setq vla_block vla_block) ; Dereference
        (princ 
          (strcat "\n  Processando Bloco (Handle: " 
                  (if (vlax-property-available-p vla_block 'Handle) 
                    (vla-get-Handle vla_block)
                    "N/A"
                  )
                  ")..."
          )
        )

        ;; Obter Ponto de Inserção Atual
        (setq current_insertion_point_vla (vl-catch-all-apply 'vla-get-InsertionPoint 
                                                              (list vla_block)
                                          )
        )
        (setq current_rotation_rad (vl-catch-all-apply 'vla-get-Rotation 
                                                       (list vla_block)
                                   )
        )

        (if 
          (or (vl-catch-all-error-p current_insertion_point_vla) 
              (vl-catch-all-error-p current_rotation_rad)
          )
          (princ (strcat "\n    AVISO: Falha ao obter Ponto de Inserção ou Rotação."))
          (progn 
            (setq current_insertion_point_list (vlax-safearray->list 
                                                 (vlax-variant-value current_insertion_point_vla)
                                               )
            )
            (setq current_rotation_rad current_rotation_rad) ; Já é o valor

            ;; Rotacionar o Bloco em 180 graus em torno do ponto de inserção
            (setq new_block_rotation_rad (+ current_rotation_rad PI))
            (setq error_obj (vl-catch-all-apply 'vla-put-Rotation 
                                                (list vla_block 
                                                      new_block_rotation_rad
                                                )
                            )
            )

            (if (vl-catch-all-error-p error_obj) 
              (princ 
                (strcat "\n    ERRO: Falha ao aplicar rotação de 180 graus: " 
                        (vl-catch-all-error-message error_obj)
                )
              )
              (progn 
                (princ 
                  (strcat "\n    Bloco rotacionado para " 
                          (angtos new_block_rotation_rad 0 4)
                          " graus."
                  )
                )

                ;; Calcular o ângulo para o deslocamento (90 graus da nova direção do bloco)
                (setq offset_angle_rad (- new_block_rotation_rad (/ PI 2.0))) ; PI/2 = 90 graus

                ;; Calcular o novo ponto de inserção
                (setq new_ins_x (+ (car current_insertion_point_list) 
                                   (* offset_distance (cos offset_angle_rad))
                                )
                )
                (setq new_ins_y (+ (cadr current_insertion_point_list) 
                                   (* offset_distance (sin offset_angle_rad))
                                )
                )
                (setq new_ins_z (caddr current_insertion_point_list)) ; Mantém o Z original
                (setq new_insertion_point_list (list new_ins_x new_ins_y new_ins_z))

                ;; Mover o Bloco (atualizar seu ponto de inserção)
                (setq error_obj (vl-catch-all-apply 'vla-put-InsertionPoint 
                                                    (list vla_block 
                                                          (vlax-3d-point new_insertion_point_list)
                                                    )
                                )
                )

                (if (vl-catch-all-error-p error_obj) 
                  (princ 
                    (strcat "\n    ERRO: Falha ao mover o bloco para o novo ponto: " 
                            (vl-catch-all-error-message error_obj)
                    )
                  )
                  (progn 
                    (princ 
                      (strcat "\n    Bloco movido para: " 
                              (vl-princ-to-string new_insertion_point_list)
                      )
                    )
                    (setq processed_count (1+ processed_count))
                  )
                )
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ;; --- Feedback Final ---
  (princ "\n-----------------------------------------------------")
  (if (> processed_count 0) 
    (princ 
      (strcat "\nSucesso! " 
              (itoa processed_count)
              " de "
              (itoa total_selected)
              " bloco(s) foram processados."
      )
    )
    (princ 
      (strcat "\nNenhum bloco foi processado com sucesso de " 
              (itoa total_selected)
              " selecionado(s). Verifique os avisos."
      )
    )
  )
  (princ "\n-----------------------------------------------------")

  ;; --- Limpeza Final ---
  (if s:old_cmdecho (setvar "CMDECHO" s:old_cmdecho))
  (if s:old_osmode (setvar "OSMODE" s:old_osmode))
  (princ "\nComando RTOB v1.0 concluído.")
  (princ)
)

;; Alias para o comando
(defun c:RTGG () (c:RotacionarGeogrelhas))
(princ)


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;   COMANDO: CRIAR CAMADAS DE BLOCO EM ESCADINHA (CBE)                   ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;
;;; Descrição: A partir de uma seleção de blocos da camada base, cria      ;;;
;;;            camadas subsequentes de blocos em formato de "escadinha",   ;;;
;;;            com offsets horizontais e verticais definidos pelo usuário. ;;;
;;;-----------------------------------------------------------------------------

;;; --- Função Auxiliar para CBE: Lógica do Modo ZIG-ZAG ---
(defun IAN:cbe-executar-zigzag (blocos_na_base blocos_deslocados mspace / 
                                direcao_escada_str direcao_sinal_z offset_h alt_v 
                                z_final total_blocos_movidos total_blocos_criados 
                                ; Vars para mover blocos deslocados
                                bloco_a_mover pt_atual_bloco rot_bloco 
                                angulo_direcao_offset_h offset_vetor_unit_x 
                                offset_vetor_unit_y novo_ponto_movido 
                                ; Vars para criar novas camadas a partir da lista unificada
                                lista_unificada vla_bloco_base pt_base_list rot_base 
                                nome_base layer_bloco_original z_atual_bloco 
                                delta_z_total qnt_novas_camadas 
                                blocos_criados_nesta_base n offset_h_acum novo_x 
                                novo_y novo_z ponto_insercao_nova_camada 
                                vla_bloco_copiado
                               ) 
  (princ "\nDetectado padrão intercalado. Executando modo ZIG-ZAG...")

  ;; --- Inputs do Usuário para o modo ZIG-ZAG ---
  (initget "Subir Descer")
  (setq direcao_escada_str (getkword "\nA escadinha será para [Subir/Descer]?: "))
  (if (null direcao_escada_str) 
    (progn (princ "\nOpção inválida. Abortado.") (exit))
  )
  (setq direcao_sinal_z (if (eq direcao_escada_str "Subir") 1.0 -1.0))

  (setq offset_h (getdist "\nDigite o offset horizontal por camada (recuo/avanço): "))
  (if (null offset_h) (progn (princ "\nOffset não fornecido. Abortado.") (exit)))

  (setq alt_v (getdist "\nDigite a altura vertical por camada (altura do degrau): "))
  (if (or (null alt_v) (<= alt_v 0.0)) 
    (progn (princ "\nAltura inválida. Abortado.") (exit))
  )

  (setq z_final (getreal "\nDigite a coordenada Z final desejada para a última camada: "))
  (if (null z_final) (progn (princ "\nZ final não fornecido. Abortado.") (exit)))

  (setq total_blocos_movidos 0)
  (setq total_blocos_criados 0)

  ;; --- Parte 1: Ajustar o recuo dos blocos já deslocados (flutuantes) ---
  (if blocos_deslocados 
    (progn 
      (princ 
        (strcat "\nProcessando " 
                (itoa (length blocos_deslocados))
                " bloco(s) deslocado(s) (aplicando recuo horizontal)..."
        )
      )
      (foreach bloco_a_mover blocos_deslocados 
        (setq pt_atual_bloco (IAN:abs-get-insertion-point-list bloco_a_mover))
        (setq rot_bloco (vla-get-Rotation bloco_a_mover))
        (setq angulo_direcao_offset_h (+ rot_bloco (/ PI 2.0)))
        (setq offset_vetor_unit_x (cos angulo_direcao_offset_h))
        (setq offset_vetor_unit_y (sin angulo_direcao_offset_h))
        (setq novo_ponto_movido (list 
                                  (+ (car pt_atual_bloco) 
                                     (* offset_h offset_vetor_unit_x)
                                  )
                                  (+ (cadr pt_atual_bloco) 
                                     (* offset_h offset_vetor_unit_y)
                                  )
                                  (caddr pt_atual_bloco)
                                )
        )
        (if 
          (not 
            (vl-catch-all-error-p 
              (vl-catch-all-apply 'vla-put-InsertionPoint 
                                  (list bloco_a_mover 
                                        (vlax-3d-point novo_ponto_movido)
                                  )
              )
            )
          )
          (setq total_blocos_movidos (1+ total_blocos_movidos))
        )
        (princ ".")
      )
    )
  )

  ;; --- Parte 2: Unificar todas as bases e criar novas camadas ---
  ;; *** LÓGICA CORRIGIDA AQUI ***
  (setq lista_unificada (append blocos_na_base blocos_deslocados))

  (if lista_unificada 
    (progn 
      (princ 
        (strcat "\nProcessando " 
                (itoa (length lista_unificada))
                " blocos totais na fundação (criando camadas subsequentes)..."
        )
      )
      (foreach vla_bloco_base lista_unificada 
        (setq pt_base_list (IAN:abs-get-insertion-point-list vla_bloco_base))
        (setq rot_base (vla-get-Rotation vla_bloco_base))
        (setq nome_base (vla-get-EffectiveName vla_bloco_base))
        (setq layer_bloco_original (vla-get-Layer vla_bloco_base))
        (setq z_atual_bloco (caddr pt_base_list))
        (setq blocos_criados_nesta_base 0)

        (setq delta_z_total (- z_final z_atual_bloco))
        (if (eq direcao_escada_str "Subir") 
          (if (<= delta_z_total 0) 
            (setq qnt_novas_camadas 0)
            (setq qnt_novas_camadas (fix (/ delta_z_total (* 2.0 alt_v))))
          )
          (if (>= delta_z_total 0) 
            (setq qnt_novas_camadas 0)
            (setq qnt_novas_camadas (fix (/ (abs delta_z_total) (* 2.0 alt_v))))
          )
        )

        (if (> qnt_novas_camadas 0) 
          (progn 
            (setq angulo_direcao_offset_h (+ rot_base (/ PI 2.0)))
            (setq offset_vetor_unit_x (cos angulo_direcao_offset_h))
            (setq offset_vetor_unit_y (sin angulo_direcao_offset_h))
            (setq n 1)
            (while (<= n qnt_novas_camadas) 
              (setq offset_h_acum (* n (* 2.0 offset_h)))
              (setq novo_x (+ (car pt_base_list) 
                              (* offset_h_acum offset_vetor_unit_x)
                           )
              )
              (setq novo_y (+ (cadr pt_base_list) 
                              (* offset_h_acum offset_vetor_unit_y)
                           )
              )
              (setq novo_z (+ z_atual_bloco (* n direcao_sinal_z (* 2.0 alt_v))))
              (setq ponto_insercao_nova_camada (list novo_x novo_y novo_z))
              (setq vla_bloco_copiado (vl-catch-all-apply 'vla-InsertBlock 
                                                          (list mspace 
                                                                (vlax-3d-point ponto_insercao_nova_camada)
                                                                nome_base
                                                                1.0
                                                                1.0
                                                                1.0
                                                                rot_base
                                                          )
                                      )
              )

              (if (not (vl-catch-all-error-p vla_bloco_copiado)) 
                (progn 
                  (vla-put-Layer vla_bloco_copiado layer_bloco_original)
                  (setq total_blocos_criados (1+ total_blocos_criados))
                  (setq blocos_criados_nesta_base (1+ blocos_criados_nesta_base))
                )
                (princ 
                  (strcat "\n    [CBE ERRO] Falha ao inserir bloco '" 
                          nome_base
                          "' na camada "
                          (itoa n)
                          ". Erro: "
                          (vl-catch-all-error-message vla_bloco_copiado)
                  )
                )
              )
              (setq n (1+ n))
            )
            (if (> blocos_criados_nesta_base 0) 
              (princ 
                (strcat " -> " 
                        (itoa blocos_criados_nesta_base)
                        " blocos adicionados."
                )
              )
            )
          )
        )
      )
    )
  )
  ;; --- Feedback Final do Modo ZIG-ZAG ---
  (princ "\n--- Resumo do Modo ZIG-ZAG ---")
  (princ (strcat "\nBlocos deslocados ajustados: " (itoa total_blocos_movidos)))
  (princ (strcat "\nNovos blocos criados: " (itoa total_blocos_criados)))
  (princ "\n---------------------------------")
)

;;; --- Função Auxiliar para CBE: Classifica blocos em 'na base' ou 'deslocados' ---
(defun IAN:cbe-classificar-blocos (lista_blocos lista_linhas_apoio mspace / 
                                   blocos_na_base blocos_deslocados circulo_temp 
                                   bloco_a_classificar pt_insercao intersec_encontrada 
                                   i linha_de_apoio intersection_points
                                  ) 
  (princ "\nClassificando blocos...")
  (setq blocos_na_base    nil
        blocos_deslocados nil
  )

  (foreach bloco_a_classificar lista_blocos 
    (setq pt_insercao (vlax-get-property bloco_a_classificar 'InsertionPoint))
    (setq circulo_temp (vla-AddCircle mspace pt_insercao 0.01))
    (setq intersec_encontrada nil)

    ;; Otimização: para de procurar assim que encontra a primeira interseção
    (setq i 0)
    (while (and (not intersec_encontrada) (< i (length lista_linhas_apoio))) 
      (setq linha_de_apoio (nth i lista_linhas_apoio))

      ;; Forma mais segura de verificar a interseção
      (setq intersection_points (vl-catch-all-apply 'vla-IntersectWith 
                                                    (list circulo_temp 
                                                          linha_de_apoio
                                                          acExtendNone
                                                    )
                                )
      )
      (if (not (vl-catch-all-error-p intersection_points)) 
        (if 
          (> 
            (vlax-safearray-get-u-bound (vlax-variant-value intersection_points) 1)
            -1
          )
          (setq intersec_encontrada T)
        )
      )
      (setq i (1+ i))
    )
    (vla-delete circulo_temp)

    (if intersec_encontrada 
      (setq blocos_na_base (cons bloco_a_classificar blocos_na_base))
      (setq blocos_deslocados (cons bloco_a_classificar blocos_deslocados))
    )
  )
  (princ " Concluído.")
  ;; Retorna uma lista com as duas listas resultantes
  (list (reverse blocos_na_base) (reverse blocos_deslocados))
)

;;; --- Função Auxiliar para CBE: Lógica do Modo Escadinha Simples ---
(defun IAN:cbe-executar-escadinha-simples (blocos_na_base mspace / 
                                           bloco_base_process_list vla_bloco_base 
                                           pt_base_list rot_base nome_base 
                                           layer_bloco_original direcao_escada_str 
                                           direcao_sinal_z offset_h alt_v z_final 
                                           total_blocos_criados bloco_info 
                                           z_atual_bloco delta_z_total 
                                           qnt_novas_camadas blocos_criados_nesta_base 
                                           angulo_direcao_offset_h offset_vetor_unit_x 
                                           offset_vetor_unit_y n offset_h_acum novo_x 
                                           novo_y novo_z ponto_insercao_nova_camada 
                                           vla_bloco_copiado
                                          ) 
  (princ "\nModo: Escadinha Simples. Todos os blocos estão na base.")

  ;; Prepara a lista de blocos no formato esperado pela lógica de processamento
  (setq bloco_base_process_list nil)
  (foreach vla_bloco_base blocos_na_base 
    (setq pt_base_list (IAN:abs-get-insertion-point-list vla_bloco_base))
    (setq rot_base (vlax-get-property vla_bloco_base 'Rotation))
    (setq nome_base (vla-get-EffectiveName vla_bloco_base))
    (setq layer_bloco_original (vla-get-Layer vla_bloco_base))
    (setq bloco_base_process_list (cons 
                                    (list vla_bloco_base pt_base_list rot_base 
                                          nome_base layer_bloco_original
                                    )
                                    bloco_base_process_list
                                  )
    )
  )
  (setq bloco_base_process_list (reverse bloco_base_process_list))

  ;; --- Inputs do Usuário ---
  (initget "Subir Descer")
  (setq direcao_escada_str (getkword "\nA escadinha será para [Subir/Descer] em relação à camada base?: "))
  (if (null direcao_escada_str) 
    (progn (princ "\nOpção inválida. Comando Cancelado.") (exit))
  )
  (setq direcao_sinal_z (if (eq direcao_escada_str "Subir") 1.0 -1.0))
  (setq offset_h (getdist "\nDigite o offset horizontal por camada (recuo/avanço): "))
  (if (null offset_h) 
    (progn (princ "\nOffset horizontal não fornecido. Comando Cancelado.") (exit))
  )
  (setq alt_v (getdist "\nDigite a altura vertical por camada (altura do bloco/degrau): "))
  (if (or (null alt_v) (<= alt_v 0.0)) 
    (progn (princ "\nAltura vertical inválida ou zero. Comando Cancelado.") (exit))
  )
  (setq z_final (getreal "\nDigite a coordenada Z final desejada para a última camada: "))
  (if (null z_final) 
    (progn (princ "\nZ final não fornecido. Comando Cancelado.") (exit))
  )

  ;; --- Processamento ---
  (setq total_blocos_criados 0)
  (princ "\nCriando camadas em escadinha...")
  (foreach bloco_info bloco_base_process_list 
    (setq vla_bloco_base       (nth 0 bloco_info)
          pt_base_list         (nth 1 bloco_info)
          rot_base             (nth 2 bloco_info)
          nome_base            (nth 3 bloco_info)
          layer_bloco_original (nth 4 bloco_info)
    )
    (setq z_atual_bloco             (caddr pt_base_list)
          blocos_criados_nesta_base 0
    )
    (setq delta_z_total (- z_final z_atual_bloco))
    (if (eq direcao_escada_str "Subir") 
      (if (<= delta_z_total 0) 
        (setq qnt_novas_camadas 0)
        (setq qnt_novas_camadas (fix (/ delta_z_total alt_v)))
      )
      (if (>= delta_z_total 0) 
        (setq qnt_novas_camadas 0)
        (setq qnt_novas_camadas (fix (/ (abs delta_z_total) alt_v)))
      )
    )
    (if (> qnt_novas_camadas 0) 
      (progn 
        (princ 
          (strcat "\n  Processando bloco base (Handle: " 
                  (vla-get-Handle vla_bloco_base)
                  "), criando "
                  (itoa qnt_novas_camadas)
                  " camada(s)..."
          )
        )
        (setq angulo_direcao_offset_h (+ rot_base (/ PI 2.0)))
        (setq offset_vetor_unit_x (cos angulo_direcao_offset_h)
              offset_vetor_unit_y (sin angulo_direcao_offset_h)
        )
        (setq n 1)
        (while (<= n qnt_novas_camadas) 
          (setq offset_h_acum (* n offset_h))
          (setq novo_x (+ (car pt_base_list) (* offset_h_acum offset_vetor_unit_x)))
          (setq novo_y (+ (cadr pt_base_list) 
                          (* offset_h_acum offset_vetor_unit_y)
                       )
          )
          (setq novo_z (+ z_atual_bloco (* n direcao_sinal_z alt_v)))
          (setq ponto_insercao_nova_camada (list novo_x novo_y novo_z))
          (setq vla_bloco_copiado (vl-catch-all-apply 'vla-InsertBlock 
                                                      (list mspace 
                                                            (vlax-3d-point ponto_insercao_nova_camada)
                                                            nome_base
                                                            1.0
                                                            1.0
                                                            1.0
                                                            rot_base
                                                      )
                                  )
          )

          ;; *** ERRO CORRIGIDO AQUI ***
          (if (not (vl-catch-all-error-p vla_bloco_copiado)) 
            (progn  ; Cláusula 'then'
                   (vla-put-Layer vla_bloco_copiado layer_bloco_original)
                   (setq total_blocos_criados (1+ total_blocos_criados))
                   (setq blocos_criados_nesta_base (1+ blocos_criados_nesta_base))
                   (princ ".")
            )
            (princ  ; Cláusula 'else'
                   (strcat "\n    [CBE ERRO] Falha ao inserir bloco '" 
                           nome_base
                           "' na camada "
                           (itoa n)
                           ". Erro: "
                           (vl-catch-all-error-message vla_bloco_copiado)
                   )
            )
          )
          (setq n (1+ n))
        )
        (if (> blocos_criados_nesta_base 0) 
          (princ 
            (strcat " " (itoa blocos_criados_nesta_base) " blocos adicionados.")
          )
        )
      )
    )
  )
  (if (> total_blocos_criados 0) 
    (princ 
      (strcat "\nSucesso! " 
              (itoa total_blocos_criados)
              " novo(s) bloco(s) criado(s)."
      )
    )
    (princ "\nNenhum novo bloco foi criado.")
  )
)

;;;
;;; c:CRIAR_CAMADAS_BLOCO_ESCADINHA (CBE) - v2.1 (Refatorada)
;;; Função principal que atua como um dispatcher, decidindo qual modo executar.
;;;
(defun c:CRIAR_CAMADAS_BLOCO_ESCADINHA (/ *error* old_cmdecho old_clayer old_osmode 
                                        ss_entidades_selecionadas i ent_name vla_ent 
                                        lista_blocos lista_linhas_apoio 
                                        listas_classificadas blocos_na_base 
                                        blocos_deslocados doc mspace
                                       ) 
  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
          (if old_clayer (setvar "CLAYER" old_clayer))
          (IAN:restore-osnap old_osmode)
        )
      )
    )
  )

  ;; --- Inicialização ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq old_clayer (getvar "CLAYER"))
  (setq old_osmode (IAN:disable-osnap))
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace (vla-get-ModelSpace doc))

  (princ "\n>>> CRIAR CAMADAS DE BLOCO EM ESCADINHA (CBE v2.1) <<<")

  ;; --- Etapa 1: Selecionar Blocos e Linhas/Polilinhas de Apoio ---
  (princ "\nSelecione os blocos base E as linhas/polilinhas de apoio:")
  (setq ss_entidades_selecionadas (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))
  (if (null ss_entidades_selecionadas) 
    (progn (princ "\nNenhuma entidade selecionada. Comando cancelado.") 
           (*error* "Seleção cancelada")
           (exit)
    )
  )

  ;; --- Etapa 2: Separar Entidades em Listas ---
  (setq lista_blocos       nil
        lista_linhas_apoio nil
  )
  (setq i 0)
  (while (< i (sslength ss_entidades_selecionadas)) 
    (setq ent_name (ssname ss_entidades_selecionadas i))
    (setq vla_ent (vlax-ename->vla-object ent_name))
    (cond 
      ((= (vla-get-ObjectName vla_ent) "AcDbBlockReference")
       (setq lista_blocos (cons vla_ent lista_blocos))
      )
      ((member (vla-get-ObjectName vla_ent) '("AcDbLine" "AcDbPolyline"))
       (setq lista_linhas_apoio (cons vla_ent lista_linhas_apoio))
      )
    )
    (setq i (1+ i))
  )
  (if (null lista_blocos) 
    (progn (princ "\nNenhum bloco encontrado na seleção.") 
           (*error* "Faltam blocos")
           (exit)
    )
  )
  (if (null lista_linhas_apoio) 
    (progn (princ "\nNenhuma linha/polilinha de apoio encontrada na seleção.") 
           (*error* "Faltam linhas de apoio")
           (exit)
    )
  )
  (princ 
    (strcat "\n" 
            (itoa (length lista_blocos))
            " bloco(s) e "
            (itoa (length lista_linhas_apoio))
            " linha(s) de apoio selecionada(s)."
    )
  )

  ;; --- Etapa 3: Classificar Blocos chamando a função auxiliar ---
  (setq listas_classificadas (IAN:cbe-classificar-blocos 
                               lista_blocos
                               lista_linhas_apoio
                               mspace
                             )
  )
  (setq blocos_na_base (car listas_classificadas))
  (setq blocos_deslocados (cadr listas_classificadas))

  ;; --- Etapa 4: Decidir o Modo de Execução (Dispatcher) ---
  (if (null blocos_deslocados) 
    ;; MODO 1: ESCADINHA SIMPLES
    (IAN:cbe-executar-escadinha-simples blocos_na_base mspace)
    ;; MODO 2: ZIG-ZAG
    (IAN:cbe-executar-zigzag blocos_na_base blocos_deslocados mspace)
  )

  ;; --- Finalização ---
  (setvar "CMDECHO" old_cmdecho)
  (setvar "CLAYER" old_clayer)
  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:CBE () (c:CRIAR_CAMADAS_BLOCO_ESCADINHA)) ; Alias curto


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;   COMANDO: ALTERAR ALTURA DE BLOCOS EM SEQUÊNCIA (ALTBLOCOSEQ)         ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;
;;; Descrição: Após selecionar um bloco inicial, este comando encontra     ;;;
;;;            outras instâncias do mesmo bloco e aplica uma alteração     ;;;
;;;            de altura em Z de forma intercalada (um sim, um não)        ;;;
;;;            seguindo a sequência do vizinho mais próximo.               ;;;
;;;-----------------------------------------------------------------------------

;; Retorna todas as instâncias VLA de um bloco pelo nome efetivo
(defun IAN:abs-get-block-instances (target_block_name / acadDoc mspace obj lst) 
  (setq lst nil)
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace (vla-get-ModelSpace acadDoc))
  (vlax-for obj mspace 
    (if 
      (and (= (vla-get-ObjectName obj) "AcDbBlockReference") 
           (= (vla-get-EffectiveName obj) target_block_name)
      )
      (setq lst (cons obj lst))
    )
  )
  (reverse lst)
)

;; Retorna o ponto de inserção (X Y Z) de um bloco VLA
(defun IAN:abs-get-insertion-point-list (block_vla / pt) 
  (if (and block_vla (vlax-property-available-p block_vla 'InsertionPoint)) 
    (progn 
      (setq pt (vl-catch-all-apply 'vlax-get-property 
                                   (list block_vla 'InsertionPoint)
               )
      )
      (if (not (vl-catch-all-error-p pt)) 
        (vlax-safearray->list (vlax-variant-value pt))
        nil
      )
    )
    nil
  )
)

;; Cria uma lista (handle . ponto) para blocos
(defun IAN:abs-build-point-map (block_list / blk h pt acc) 
  (setq acc nil)
  (foreach blk block_list 
    (setq h (if (vlax-property-available-p blk 'Handle) (vla-get-Handle blk) nil))
    (setq pt (IAN:abs-get-insertion-point-list blk))
    (if (and h pt) (setq acc (cons (cons h pt) acc)))
  )
  (reverse acc)
)

;; Ordena blocos por proximidade a partir de um bloco inicial
(defun IAN:abs-sort-blocks-by-proximity (first_block_vla candidate_blocks_vla 
                                         block_point_map / ordered_list 
                                         current_block_obj rem candidates pt curpt 
                                         next min_dist candpt dist
                                        ) 
  (setq ordered_list nil)
  (setq current_block_obj first_block_vla)
  (setq rem (append candidate_blocks_vla nil))
  (while current_block_obj 
    (setq ordered_list (cons current_block_obj ordered_list))
    (setq rem (vl-remove-if 
                (function 
                  (lambda (blk) 
                    (equal (vla-get-Handle blk) (vla-get-Handle current_block_obj))
                  )
                )
                rem
              )
    )
    (if rem 
      (progn 
        (setq curpt (cdr (assoc (vla-get-Handle current_block_obj) block_point_map)))
        (setq next     nil
              min_dist nil
        )
        (if curpt 
          (foreach cand rem 
            (setq candpt (cdr (assoc (vla-get-Handle cand) block_point_map)))
            (if candpt 
              (progn 
                (setq dist (distance curpt candpt))
                (if (or (null min_dist) (< dist min_dist)) 
                  (progn (setq min_dist dist) (setq next cand))
                )
              )
            )
          )
        )
        (setq current_block_obj next)
      )
      (setq current_block_obj nil)
    )
  )
  (if ordered_list (reverse ordered_list) nil)
)

;; Comando: altera altura Z de blocos em sequência (um sim, um não)
(defun c:ALTBLOCOSEQ (/ *error* old_cmdecho old_osmode first_block_ename 
                      first_block_vla first_block_name height_offset 
                      all_instances_of_block ordered_block_sequence block_vla_in_loop 
                      current_insertion_pt_list new_insertion_pt_list 
                      vla_operation_error_obj modified_block_count point_map
                     ) 
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
          (IAN:restore-osnap old_osmode)
        )
      )
    )
  )
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq old_osmode (IAN:disable-osnap))
  (setq modified_block_count 0)
  (princ "\n>>> ALTERAR ALTURA DE BLOCOS EM SEQUÊNCIA (ALTBLOCOSEQ) <<<")
  ;; Seleciona bloco inicial
  (setq first_block_vla nil)
  (while (not first_block_vla) 
    (setq first_block_ename (car (entsel "\nSelecione o PRIMEIRO bloco da sequência: ")))
    (cond 
      ((null first_block_ename)
       (princ "\nSeleção cancelada. Comando encerrado.")
       (setvar "CMDECHO" old_cmdecho)
       (IAN:restore-osnap old_osmode)
       (exit)
      )
      ((setq vla_operation_error_obj (vl-catch-all-apply 'vlax-ename->vla-object 
                                                         (list first_block_ename)
                                     )
       )
       (if (vl-catch-all-error-p vla_operation_error_obj) 
         (princ 
           (strcat "\n[ABS ERRO] Falha ao obter objeto VLA: " 
                   (vl-catch-all-error-message vla_operation_error_obj)
           )
         )
         (progn 
           (setq first_block_vla vla_operation_error_obj)
           (if 
             (not 
               (and (vlax-property-available-p first_block_vla 'ObjectName) 
                    (= (vla-get-ObjectName first_block_vla) "AcDbBlockReference")
               )
             )
             (progn (princ "\nA entidade selecionada não é um bloco. Tente novamente.") 
                    (setq first_block_vla nil)
             )
             (setq first_block_name (vla-get-EffectiveName first_block_vla))
           )
         )
       )
      )
    )
  )
  (princ 
    (strcat "\nBloco inicial selecionado: '" 
            first_block_name
            "' (Handle: "
            (vla-get-Handle first_block_vla)
            ")"
    )
  )
  ;; Altura de deslocamento
  (setq height_offset (getreal "\nDigite a altura de deslocamento Z para blocos alternados (ex: 0.5): "))
  (if (or (null height_offset) (equal height_offset 0.0 1e-8)) 
    (progn 
      (princ "\nAltura de deslocamento inválida ou zero. Comando cancelado.")
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  ;; Coleta instâncias e ordena
  (setq all_instances_of_block (IAN:abs-get-block-instances first_block_name))
  (if (or (null all_instances_of_block) (< (length all_instances_of_block) 1)) 
    (progn 
      (princ 
        (strcat "\nNenhuma instância adicional do bloco '" 
                first_block_name
                "' encontrada."
        )
      )
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ 
    (strcat "\nEncontradas " 
            (itoa (length all_instances_of_block))
            " instâncias do bloco '"
            first_block_name
            "'."
    )
  )

  (setq point_map (IAN:abs-build-point-map all_instances_of_block))
  (princ "\nOrdenando blocos por proximidade...")
  (setq ordered_block_sequence (IAN:abs-sort-blocks-by-proximity 
                                 first_block_vla
                                 all_instances_of_block
                                 point_map
                               )
  )
  (if (or (null ordered_block_sequence) (< (length ordered_block_sequence) 2)) 
    (progn 
      (princ "\nNão foi possível ordenar uma sequência de pelo menos 2 blocos.")
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ 
    (strcat "\n" 
            (itoa (length ordered_block_sequence))
            " blocos ordenados na sequência."
    )
  )

  ;; Aplica altura intercalada
  (princ "\nAplicando altura intercalada...")
  (setq idx               0
        next_index        0
        last_displaced    nil
        z_anterior        nil
        dois_vezes_offset (* 2 height_offset)
        tol               1e-8
  )
  (foreach block_vla_in_loop ordered_block_sequence 
    (setq current_insertion_pt_list (cdr 
                                      (assoc (vla-get-Handle block_vla_in_loop) 
                                             point_map
                                      )
                                    )
    )
    (if current_insertion_pt_list 
      (progn 
        (setq z_atual (caddr current_insertion_pt_list))
        (if (and z_anterior (not (equal z_atual z_anterior tol))) 
          (progn 
            (setq delta (- z_atual z_anterior))
            (setq deltaA (abs delta))
            (setq quoc (/ deltaA dois_vezes_offset))
            (setq divisivel? (equal (- quoc (fix quoc)) 0.0 tol))
            (cond 
              ((and divisivel? last_displaced) (setq next_index 0))
              ((and divisivel? (not last_displaced)) (setq next_index 1))
              ((and (not divisivel?) last_displaced) (setq next_index 1))
              ((and (not divisivel?) (not last_displaced)) (setq next_index 0))
            )
          )
        )
        (if (= next_index 1) 
          (progn 
            (setq new_insertion_pt_list (list (car current_insertion_pt_list) 
                                              (cadr current_insertion_pt_list)
                                              (+ z_atual height_offset)
                                        )
            )
            (setq vla_operation_error_obj (vl-catch-all-apply 'vla-put-InsertionPoint 
                                                              (list block_vla_in_loop 
                                                                    (vlax-3d-point new_insertion_pt_list)
                                                              )
                                          )
            )
            (if (not (vl-catch-all-error-p vla_operation_error_obj)) 
              (setq modified_block_count (1+ modified_block_count))
            )
            (setq last_displaced T)
          )
          (setq last_displaced nil)
        )
        (setq next_index (- 1 next_index))
        (setq z_anterior z_atual)
      )
      (princ 
        (strcat "\n  [ABS AVISO] Falha ao obter ponto do bloco (Handle: " 
                (vla-get-Handle block_vla_in_loop)
                ")"
        )
      )
    )
    (setq idx (1+ idx))
  )
  (if (> modified_block_count 0) 
    (princ 
      (strcat "\nSucesso! " 
              (itoa modified_block_count)
              " bloco(s) tiveram a altura modificada."
      )
    )
    (princ "\nNenhum bloco teve a altura modificada.")
  )
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:ABS () (c:ALTBLOCOSEQ))


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;   COMANDO: APAGAR BLOCOS INTERCALADOS EM SEQUÊNCIA (INVERSO)           ;;;
;;;            (DELBLOCINV)                                                ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;
;;; Descrição: Após selecionar um bloco inicial, este comando encontra     ;;;
;;;            outras instâncias do mesmo bloco no ModelSpace e apaga      ;;;
;;;            blocos de forma intercalada (o primeiro sim, o segundo não, ;;;
;;;            o terceiro sim, etc.) seguindo a sequência do vizinho       ;;;
;;;            mais próximo.                                               ;;;
;;;-----------------------------------------------------------------------------

;;;
;;; c:APAGAR_BLOCOS_INTERCALADOS_INVERSO (DELBLOCINV)
;;;
(defun c:APAGAR_BLOCOS_INTERCALADOS_INVERSO (/ *error* ; Manipulador de erro local
                                             old_cmdecho old_osmode ; Para restaurar CMDECHO e OSMODE
                                             first_block_ename first_block_vla 
                                             first_block_name ; Bloco inicial
                                             all_instances_of_block ; Lista de todos os blocos com o mesmo nome
                                             ordered_block_sequence ; Blocos ordenados por proximidade
                                             block_vla_in_loop index ; Para iteração
                                             vla_operation_error_obj ; Para capturar erros de operações VLA
                                             deleted_block_count ; Contador
                                             point_map ; Cache de pontos de inserção
                                            ) 

  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
          (IAN:restore-osnap old_osmode)
        )
      )
    )
  )

  ;; --- Inicialização ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; Suprime eco de comandos
  (setq old_osmode (IAN:disable-osnap))
  (setq deleted_block_count 0)

  (princ "\n>>> APAGAR BLOCOS INTERCALADOS EM SEQUÊNCIA (ORDEM INVERSA) <<<")

  ;; --- Etapa 1: Selecionar o Primeiro Bloco ---
  (setq first_block_vla nil)
  (while (not first_block_vla) 
    (setq first_block_ename (car (entsel "\nSelecione o PRIMEIRO bloco da sequência para iniciar a remoção: ")))
    (cond 
      ((null first_block_ename)
       (princ "\nSeleção cancelada. Comando encerrado.")
       (setvar "CMDECHO" old_cmdecho)
       (IAN:restore-osnap old_osmode)
       (exit)
      )
      ((setq vla_operation_error_obj (vl-catch-all-apply 'vlax-ename->vla-object 
                                                         (list first_block_ename)
                                     )
       )
       (if (vl-catch-all-error-p vla_operation_error_obj) 
         (princ 
           (strcat "\n[DELBLOCINV ERRO] Falha ao obter objeto VLA: " 
                   (vl-catch-all-error-message vla_operation_error_obj)
           )
         )
         (progn 
           (setq first_block_vla vla_operation_error_obj)
           (if 
             (not 
               (and (vlax-property-available-p first_block_vla 'ObjectName) 
                    (= (vla-get-ObjectName first_block_vla) "AcDbBlockReference")
               )
             )
             (progn 
               (princ "\nA entidade selecionada não é um bloco. Tente novamente.")
               (setq first_block_vla nil)
             )
             (setq first_block_name (vla-get-EffectiveName first_block_vla))
           )
         )
       )
      )
    )
  )
  (princ 
    (strcat "\nBloco inicial selecionado: '" 
            first_block_name
            "' (Handle: "
            (vla-get-Handle first_block_vla)
            ")"
    )
  )

  ;; --- Etapa 2: Coletar Todas as Instâncias do Mesmo Bloco ---
  ;; Reutiliza a função auxiliar IAN:abs-get-block-instances
  (setq all_instances_of_block (IAN:abs-get-block-instances first_block_name))
  (if (or (null all_instances_of_block) (< (length all_instances_of_block) 1)) 
    (progn 
      (princ 
        (strcat "\nNenhuma instância adicional do bloco '" 
                first_block_name
                "' encontrada (ou apenas a selecionada)."
        )
      )
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ 
    (strcat "\nEncontradas " 
            (itoa (length all_instances_of_block))
            " instâncias totais do bloco '"
            first_block_name
            "'."
    )
  )

  ;; Gera mapa de pontos para evitar leituras repetidas
  (setq point_map (IAN:abs-build-point-map all_instances_of_block))

  ;; --- Etapa 3: Ordenar Blocos por Proximidade ---
  ;; Reutiliza a função auxiliar IAN:abs-sort-blocks-by-proximity
  (princ "\nOrdenando blocos por proximidade...")
  (setq ordered_block_sequence (IAN:abs-sort-blocks-by-proximity 
                                 first_block_vla
                                 all_instances_of_block
                                 point_map
                               )
  )

  (if (null ordered_block_sequence)  ; Não precisa verificar < 2 aqui, pois até 1 bloco pode ser apagado se for o primeiro.
    (progn 
      (princ "\nNão foi possível ordenar a sequência de blocos. Verifique os blocos.")
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ 
    (strcat "\n" 
            (itoa (length ordered_block_sequence))
            " blocos ordenados na sequência."
    )
  )

  ;; --- (NOVO) pedir altura de referência igual à da ABS ---
  (setq height_offset (getreal "\nInforme o mesmo deslocamento Z usado na ABS (ex: 0.5): "))
  (if (or (null height_offset) (<= (abs height_offset) 1e-8)) 
    (progn 
      (princ "\nValor inválido. Comando cancelado.")
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )

  ;; --- Etapa 4: Remoção intercalada adaptativa -----------------
  (princ "\nRemovendo blocos com lógica adaptativa...")

  (setq idx 0) ;; índice global
  (setq next_index 0) ;; 0 = apaga, 1 = mantém
  (setq last_deleted nil) ;; T se o bloco anterior foi apagado
  (setq z_anterior nil)
  (setq dois_vezes_offset (* 2 height_offset))
  (setq tol 1e-8)

  (foreach block_vla_in_loop ordered_block_sequence 
    (setq current_pt (cdr (assoc (vla-get-Handle block_vla_in_loop) point_map)))

    (if current_pt 
      (progn 
        (setq z_atual (caddr current_pt))

        ;; --- Mudança de nível? ---------------------------------
        (if (and z_anterior (not (equal z_atual z_anterior tol))) 
          (progn 
            (setq delta (- z_atual z_anterior))
            (setq divisivel? (equal 
                               (- (/ (abs delta) dois_vezes_offset) 
                                  (fix (/ (abs delta) dois_vezes_offset))
                               )
                               0.0
                               tol
                             )
            )

            ;; -------- aplica as 4 regras --------
            (cond 
              ((and divisivel? last_deleted) (setq next_index 1)) ; começa em 1 (mantém)
              ((and divisivel? (not last_deleted)) (setq next_index 0)) ; começa em 0 (apaga)
              ((and (not divisivel?) last_deleted) (setq next_index 0)) ; começa em 0 (apaga)
              ((and (not divisivel?) (not last_deleted)) (setq next_index 1)) ; começa em 1 (mantém)
            )
          )
        )

        ;; --- Apaga ou não de acordo com next_index -------------
        (if (= next_index 0) 
          (progn 
            (setq vla_operation_error_obj (vl-catch-all-apply 'vla-Delete 
                                                              (list block_vla_in_loop)
                                          )
            )
            (if (vl-catch-all-error-p vla_operation_error_obj) 
              (princ 
                (strcat "\n  [DELBLOCINV AVISO] Falha ao apagar bloco (Handle: " 
                        (vla-get-Handle block_vla_in_loop)
                        "): "
                        (vl-catch-all-error-message vla_operation_error_obj)
                )
              )
              (setq deleted_block_count (1+ deleted_block_count))
            )
            (setq last_deleted T)
          )
          (setq last_deleted nil)
        )

        ;; --- Alterna para o próximo bloco ----------------------
        (setq next_index (- 1 next_index)) ; 0?1
        (setq z_anterior z_atual)
      )
      (princ 
        (strcat "\n  [DELBLOCINV AVISO] Falha ao obter ponto (Handle: " 
                (vla-get-Handle block_vla_in_loop)
                ")."
        )
      )
    )
    (setq idx (1+ idx))
  )

  ;; --- Etapa 5: Feedback Final ---
  (if (> deleted_block_count 0) 
    (princ 
      (strcat "\nSucesso! " 
              (itoa deleted_block_count)
              " bloco(s) '"
              first_block_name
              "' foram apagados."
      )
    )
    (princ "\nNenhum bloco foi apagado (verifique a sequência).")
  )

  ;; --- Finalização ---
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)
;;; Alias curto para o comando
(defun c:DELBLOCINV () (c:APAGAR_BLOCOS_INTERCALADOS_INVERSO))


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;   COMANDO: EXPORTAR DADOS DE BLOCOS (X,Y,Z,Ângulo) (EXDBLK)            ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;
;;; Descrição: Seleciona instâncias de bloco e exporta seus pontos de      ;;;
;;;            inserção (X,Y,Z) e ângulo de rotação (em RADIANOS) para     ;;;
;;;            um arquivo TXT, no formato: X,Y,Z,ANGULO                    ;;;
;;;-----------------------------------------------------------------------------

;;;
;;; c:EXPORTAR_DADOS_BLOCOS (EXDBLK)
;;;
(defun c:EXPORTAR_DADOS_BLOCOS (/ *error* old_cmdecho old_osmode ss_blocks 
                                selection_count i ent_name vla_block pt_variant 
                                pt_safearray pt_list x y z rotation_rad file_path f 
                                export_count
                               ) 

  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
          (setq f (IAN:close-handle f))
          (IAN:restore-osnap old_osmode)
        )
      )
    )
  )

  ;; --- Inicialização ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; Suprime eco de comandos
  (setq old_osmode (IAN:disable-osnap))
  (setq export_count 0)

  (princ "\n>>> EXPORTAR DADOS DE BLOCOS (X,Y,Z,Ângulo) (EXDBLK) <<<")

  ;; --- Etapa 1: Selecionar Blocos ---
  (princ "\nSelecione os blocos para exportar os dados:")
  (setq ss_blocks (ssget '((0 . "INSERT")))) ; Filtra para selecionar apenas blocos

  (if (null ss_blocks) 
    (progn (princ "\nNenhum bloco selecionado. Comando cancelado.") 
           (setvar "CMDECHO" old_cmdecho)
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )
  (setq selection_count (sslength ss_blocks))
  (princ 
    (strcat "\n" 
            (itoa selection_count)
            " bloco(s) selecionado(s) para exportação."
    )
  )

  ;; --- Etapa 2: Obter Arquivo de Saída ---
  (setq file_path (getfiled "Salvar arquivo de dados dos blocos TXT" "" "txt" 1)) ; 1 = Salvar (pode sobrescrever)
  (if (null file_path) 
    (progn (princ "\nNenhum arquivo selecionado para salvar. Comando cancelado.") 
           (setvar "CMDECHO" old_cmdecho)
           (IAN:restore-osnap old_osmode)
           (exit)
    )
  )

  ;; --- Etapa 3: Processar Blocos e Escrever no Arquivo ---
  (setq f (IAN:safe-open file_path "w"))
  (if (not f) 
    (progn 
      (princ 
        (strcat "\n[EXDBLK ERRO] Não foi possível criar/abrir o arquivo: " 
                file_path
        )
      )
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )

  (princ (strcat "\nExportando dados para: " file_path " ..."))
  (setq i 0)
  (while (< i selection_count) 
    (setq ent_name (ssname ss_blocks i))
    (setq vla_block (vl-catch-all-apply 'vlax-ename->vla-object (list ent_name)))

    (if (not (vl-catch-all-error-p vla_block)) 
      (progn 
        (setq vla_block vla_block) ; Já é o objeto VLA

        ;; Obter Ponto de Inserção
        (setq pt_variant (vlax-get-property vla_block 'InsertionPoint))
        (setq pt_safearray (vlax-variant-value pt_variant))
        (setq pt_list (vlax-safearray->list pt_safearray))
        (setq x (car pt_list))
        (setq y (cadr pt_list))
        (setq z (caddr pt_list))

        ;; Obter Rotação (em Radianos)
        (setq rotation_rad (vlax-get-property vla_block 'Rotation))

        ;; Formatar e Escrever Linha
        ;; Usar rtos com precisão alta para coordenadas e rotação
        (write-line 
          (strcat 
            (rtos x 2 8)
            ","
            (rtos y 2 8)
            ","
            (rtos z 2 8)
            ","
            (rtos rotation_rad 2 8) ; Ângulo em radianos com 8 casas decimais
          )
          f
        )
        (setq export_count (1+ export_count))
        (princ ".")
      )
      (princ 
        (strcat "\n  [EXDBLK AVISO] Falha ao processar um bloco selecionado: " 
                (vl-catch-all-error-message vla_block)
        )
      )
    )
    (setq i (1+ i))
  )
  (setq f (IAN:close-handle f))

  ;; --- Feedback Final ---
  (if (> export_count 0) 
    (princ 
      (strcat "\nSucesso! Dados de " 
              (itoa export_count)
              " bloco(s) exportados para "
              file_path
              "."
      )
    )
    (princ "\nNenhum dado de bloco foi exportado.")
  )

  ;; --- Finalização ---
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:EXDBLK () (c:EXPORTAR_DADOS_BLOCOS)) ; Alias curto




;;;; ----------------------------------------------------------------------;;;;
;;; 								Comandos Separados por tipo de muro                    ;;;
;;;; ----------------------------------------------------------------------;;;;

;;; --- Função de Desenho para o Menu Gráfico (v2.0 - usando ENTMAKE) ---
(defun IAN:desenhar-menu-grafico (titulo opcoes p1 p2 item_height item_realcado texto_size /
                                  entidades_de_texto i start_y texto_y texto_x cor)
  
  ;; Desenha a borda e o fundo realçado com grdraw, que funciona.
  (grdraw p1 p2 7) (grdraw p2 p3 7) (grdraw p3 p4 7) (grdraw p4 p1 7)
  (if (and item_realcado (>= item_realcado 0))
    (progn
      (setq start_y (- (cadr p1) (* item_height 1.5)))
      (setq texto_y (- start_y (* item_realcado item_height)))
      (grdraw (list (car p1) (+ texto_y item_height)) (list (car p2) (+ texto_y item_height)) 5)
      (grdraw (list (car p1) texto_y) (list (car p2) texto_y) 5)
    )
  )

  ;; --- CRIA ENTIDADES DE TEXTO REAIS ---
  (setq entidades_de_texto nil)
  
  ;; Cria a entidade de Título
  (entmake (list '(0 . "TEXT")
                 (cons 10 (list (car p1) (+ (cadr p1) (* item_height 0.3)) 0))
                 (cons 40 texto_size)
                 (cons 1 (strcat " " titulo))
                 '(7 . "STANDARD") ; Estilo de texto
                 '(62 . 6) ; Cor (Magenta)
           )
  )
  (setq entidades_de_texto (cons (entlast) entidades_de_texto))

  ;; Cria as entidades das Opções
  (setq i 0)
  (setq start_y (- (cadr p1) (* item_height 1.5)))
  (setq texto_x (+ (car p1) (* item_height 0.5)))
  (foreach opcao opcoes
    (setq texto_y (- start_y (* i item_height)))
    (setq cor (if (= i item_realcado) 7 255)) ; Branco se realçado, Padrão se não
    
    (entmake (list '(0 . "TEXT")
                   (cons 10 (list texto_x texto_y 0))
                   (cons 40 texto_size)
                   (cons 1 (strcat " " opcao))
                   '(7 . "STANDARD")
                   (cons 62 cor)
             )
    )
    (setq entidades_de_texto (cons (entlast) entidades_de_texto))
    (setq i (1+ i))
  )
  
  (reverse entidades_de_texto) ; Retorna a lista de entidades criadas
)


;;; --- Função Principal do Menu Gráfico (v2.0 - usando ENTMAKE) ---
(defun IAN:menu-grafico (titulo opcoes /
                         view_center view_height box_width box_height half_width half_height
                         p1 p2 p3 p4 item_height start_y i texto_y texto_x
                         looping item_selecionado ultimo_item_realcado mouse_data codigo_input ponto_mouse
                         entidades_menu_atual
                        )
  ;; --- 1. Calcular as Dimensões e Posição do Menu ---
  (setq view_center (getvar "VIEWCTR"))
  (setq view_height (getvar "VIEWSIZE"))
  (setq texto_size (* view_height 0.03))
  (setq box_width (* view_height 0.5))
  (setq item_height (* texto_size 1.5))
  (setq box_height (* item_height (+ (length opcoes) 2)))
  (setq half_width (/ box_width 2.0))
  (setq half_height (/ box_height 2.0))

  (setq p1 (list (- (car view_center) half_width) (+ (cadr view_center) half_height)))
  (setq p2 (list (+ (car view_center) half_width) (+ (cadr view_center) half_height)))
  (setq p3 (list (+ (car view_center) half_width) (- (cadr view_center) half_height)))
  (setq p4 (list (- (car view_center) half_width) (- (cadr view_center) half_height)))

  ;; --- 2. Loop Principal de Interação ---
  (setq looping T ultimo_item_realcado -1 entidades_menu_atual nil)
  (while looping
    ;; Apaga as entidades de texto antigas, se existirem
    (if entidades_menu_atual
      (foreach ent entidades_menu_atual (if (entget ent) (entdel ent)))
    )
    (redraw) ; Limpa os gráficos temporários (realce)

    ;; Desenha o novo menu e guarda as entidades de texto criadas
    (setq entidades_menu_atual (IAN:desenhar-menu-grafico titulo opcoes p1 p2 item_height ultimo_item_realcado texto_size))
    
    (setq mouse_data (grread T 15 0))
    (setq codigo_input (car mouse_data))
    (setq ponto_mouse (cadr mouse_data))

    (cond
      ((= codigo_input 5) ; Mouse moveu
       (setq i 0 item_selecionado -1)
       (setq start_y (- (cadr p1) (* item_height 1.5)))
       (while (and (< i (length opcoes)) (= item_selecionado -1))
         (setq texto_y (- start_y (* i item_height)))
         (if (and ponto_mouse
                  (<= (car ponto_mouse) (car p2)) (>= (car ponto_mouse) (car p1))
                  (<= (cadr ponto_mouse) (+ texto_y item_height)) (>= (cadr ponto_mouse) texto_y))
           (setq item_selecionado i)
         )
         (setq i (1+ i))
       )
       (if (/= item_selecionado ultimo_item_realcado)
         (setq ultimo_item_realcado item_selecionado)
       )
      )
      ((= codigo_input 3) ; Mouse clicou
       (if (and ultimo_item_realcado (>= ultimo_item_realcado 0))
         (progn
           (setq looping nil)
           (setq item_selecionado (nth ultimo_item_realcado opcoes))
         )
       )
      )
      ((= codigo_input 2) ; Tecla pressionada (ESC)
       (setq looping nil item_selecionado nil)
      )
    )
  )
  
  ;; Limpeza final: apaga as últimas entidades de texto criadas
  (if entidades_menu_atual
    (foreach ent entidades_menu_atual (if (entget ent) (entdel ent)))
  )
  (redraw)
  item_selecionado
)

;;;
;;; c:CONTENCOES
;;; Comando principal que exibe o menu gráfico e chama a rotina apropriada.
;;;
(defun c:CONTENCOES (/ lista_de_tipos escolha) 

  ;; 1. Definir as opções do menu - *** STRINGS CORRIGIDAS SEM ACENTOS ***
  (setq lista_de_tipos '("Terramesh" "Terra Armada" "Lock & Load" "Geogrelha"))

  ;; 2. Chamar a função de menu gráfico - *** TÍTULO CORRIGIDO SEM ACENTOS ***
  (setq escolha (IAN:menu-grafico "Selecione o Tipo de Contencao" lista_de_tipos))

  ;; 3. Agir com base na escolha do usuário
  (if escolha 
    (progn 
      (princ (strcat "\nIniciando rotina para: " escolha))
      ;; --- DISPATCHER ---
      (cond 
        ((= escolha "Terramesh")
         (c:CONSTRUIR_MURO_TERRAMESH)
        )
        ((= escolha "Terra Armada")
         (c:CONSTRUIR_MURO_TERRA_ARMADA)
        )
        ((= escolha "Lock & Load")
         (c:CONSTRUIR_MURO_LOCKNLOAD)
        )
        ((= escolha "Geogrelha")
         (c:CONSTRUIR_MURO_GEOGRELHA)
        )
      )
    )
    (princ "\nComando cancelado.")
  )
  (princ)
)


;;; ------------------------------------------------------------------------
;;; --- Funções de Construção Específicas (Placeholders) ---
;;; ------------------------------------------------------------------------

(defun c:CONSTRUIR_MURO_TERRAMESH (/) 
  (princ "\n(Lógica de construção para Terramesh vai aqui...)")
  (princ)
)

(defun c:CONSTRUIR_MURO_TERRA_ARMADA (/) 
  (princ "\n(Lógica de construção para Terra Armada vai aqui...)")
  (princ)
)

(defun c:CONSTRUIR_MURO_LOCKNLOAD (/) 
  (princ "\n(Lógica de construção para Lock & Load vai aqui...)")
  (princ)
)

(defun c:CONSTRUIR_MURO_GEOGRELHA (/) 
  (princ "\n(Lógica de construção para Geogrelha vai aqui...)")
  (princ)
)










;;;; ----------------------------------------------------------------------;;;;
;;; Mensagem de Carregamento Final                                         ;;;
;;;; ----------------------------------------------------------------------;;;;
(princ "\nComandos LSP contenções carregados:")
(princ "\n  COMANDOS")
(princ "\n  GERAR_PONTOS_REGUA (GPR)")
(princ "\n  COLOCAR_BLOCO_EM_PONTOS_MEDIOS (CBPM)")
(princ "\n  ALTBLOCOSEQ (ABS)") 
(princ "\n  APAGAR_BLOCOS_INTERCALADOS_INVERSO (DELBLOCINV)") 
(princ "\n  EXPORTAR_DADOS_BLOCOS (EXDBLK)") 
(princ "\nDigite o nome do comando ou seu alias (entre parênteses) para usar.")
(princ)

