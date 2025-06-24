;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;                           Contencoes LSP                               ;;;
;;;                                                                        ;;;
;;;------------------------------------------------------------------------;;;
;;;                                                                        ;;;
;;; Autor: ian junji                                                       ;;;
;;; Versão: 0.1                                                            ;;;
;;;                                                                        ;;;
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

;;; --- Função "Core" para GPR: A lógica pura, sem interação com o usuário ---
;;; Recebe os parâmetros e executa a geração de pontos.
;;; Retorna T para sucesso, nil para falha.
(defun IAN:gerar-pontos-regua-core (ss_linhas_alvo comprimento_regua arquivo_saida / 
                                    *error* contador ent_nome ent_obj 
                                    lista_todos_os_pontos f ponto_A ponto_B 
                                    lista_pontos_entidade_atual 
                                    primeiro_ponto_adicionado tolerancia 
                                    start_pt_entidade_catch
                                   ) 
  (defun *error* (msg) 
    (IAN:std-error msg (function (lambda () (setq f (IAN:close-handle f)))))
  )

  (setq tolerancia 0.0001)
  (setq lista_todos_os_pontos nil)
  (setq contador 0)
  (princ 
    (strcat "\nProcessando " 
            (itoa (sslength ss_linhas_alvo))
            " entidade(s) com régua de "
            (rtos comprimento_regua)
            "..."
    )
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
      (progn (princ (strcat "\n  [AVISO] Falha ao obter ponto inicial. Pulando.")) 
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
          T ; Retorna T para sucesso
        )
        (progn 
          (princ 
            (strcat "\n[ERRO] Falha ao abrir arquivo para escrita: " arquivo_saida)
          )
          nil
        )
      )
    )
    (progn (princ "\n\nNenhum ponto gerado para salvar.") nil)
  )
)

;;; --- Função Auxiliar para GPR: Encontrar Próximo Ponto com Régua  ---
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

;;; --- Comando Principal: GERAR_PONTOS_REGUA (Interface com o Usuário) ---
(defun c:GERAR_PONTOS_REGUA (/ *error* old_osmode ss_linhas_alvo comprimento_regua 
                             arquivo_saida
                            ) 
  (defun *error* (msg) 
    (IAN:std-error msg (function (lambda () (IAN:restore-osnap old_osmode))))
  )
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> GERADOR DE PONTOS COM RÉGUA (GPR) <<<")
  (princ "\nSelecione as entidades alvo (linhas, polilinhas, arcos, splines): ")
  (setq ss_linhas_alvo (ssget '((0 . "LINE,LWPOLYLINE,ARC,SPLINE"))))

  (if ss_linhas_alvo 
    (progn 
      (while (or (null comprimento_regua) (<= comprimento_regua 0.0)) 
        (setq comprimento_regua (getdist "\nDigite o comprimento da régua/segmento: "))
        (if (or (null comprimento_regua) (<= comprimento_regua 0.0)) 
          (princ "\nComprimento inválido. Deve ser positivo.")
        )
      )
      (setq arquivo_saida (getfiled "Salvar arquivo de coordenadas TXT" "" "txt" 1))

      ;; Se o usuário forneceu todos os dados, chama a função "Motor"
      (if arquivo_saida 
        (IAN:gerar-pontos-regua-core 
          ss_linhas_alvo
          comprimento_regua
          arquivo_saida
        )
        (princ "\nNenhum arquivo selecionado. Comando cancelado.")
      )
    )
    (princ "\nNenhuma entidade selecionada. Comando cancelado.")
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

;;; --- Função "Core" para CBPM: A lógica pura, sem interação com o usuário ---
(defun IAN:cbpm-core (arquivo_txt nome_bloco_real / *error* f linha lineno 
                      lista_pontos_lidos p_temp i p_orig_A p_orig_B p_medio_calc 
                      lista_segmentos_para_blocos layer_alvo_nome vla_layer_alvo 
                      angulo_insercao old_clayer contador_blocos_inseridos mspace doc 
                      tolerancia_dist vla_bloco_inserido_raw segmento 
                      first_block_vla_obj
                     ) 
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (setq f (IAN:close-handle f))
          (if old_clayer (setvar "CLAYER" old_clayer))
        )
      )
    )
  )

  (setq first_block_vla_obj nil)
  (setq layer_alvo_nome "BLOCOS_PONTOS_MEDIOS")
  (setq contador_blocos_inseridos 0)
  (setq lista_pontos_lidos nil)
  (setq lista_segmentos_para_blocos nil)
  (setq tolerancia_dist 0.00001)
  (setq z_tolerance 0.01)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace (vla-get-ModelSpace doc))
  (setq old_clayer (getvar "CLAYER"))

  (setq f (IAN:safe-open arquivo_txt "r"))
  (if (not f) 
    (progn 
      (princ 
        (strcat "\n[ERRO] Falha ao abrir arquivo para CBPM-Core: " arquivo_txt)
      )
      (exit)
    )
  )

  (princ (strcat "\nLendo arquivo de pontos: " arquivo_txt))
  (setq lineno 0)
  (while (setq linha (read-line f)) 
    (setq lineno (1+ lineno))
    (setq p_temp (IAN:cbpm-string-to-pointlist linha))
    (if p_temp (setq lista_pontos_lidos (cons p_temp lista_pontos_lidos)))
  )
  (setq f (IAN:close-handle f))
  (setq lista_pontos_lidos (reverse lista_pontos_lidos))

  (if (< (length lista_pontos_lidos) 2) 
    (progn (princ "\n[ERRO] Menos de 2 pontos válidos lidos.") (exit))
  )

  (setq i 0)
  (while (< i (1- (length lista_pontos_lidos))) 
    (setq p_orig_A (nth i lista_pontos_lidos))
    (setq p_orig_B (nth (1+ i) lista_pontos_lidos))
    (if (<= (abs (- (caddr p_orig_A) (caddr p_orig_B))) z_tolerance) 
      (if (> (distance p_orig_A p_orig_B) tolerancia_dist) 
        (setq lista_segmentos_para_blocos (cons 
                                            (list 
                                              (list 
                                                (/ 
                                                  (+ (car p_orig_A) (car p_orig_B))
                                                  2.0
                                                )
                                                (/ 
                                                  (+ (cadr p_orig_A) 
                                                     (cadr p_orig_B)
                                                  )
                                                  2.0
                                                )
                                                (/ 
                                                  (+ (caddr p_orig_A) 
                                                     (caddr p_orig_B)
                                                  )
                                                  2.0
                                                )
                                              )
                                              p_orig_A
                                              p_orig_B
                                            )
                                            lista_segmentos_para_blocos
                                          )
        )
      )
    )
    (setq i (1+ i))
  )
  (setq lista_segmentos_para_blocos (reverse lista_segmentos_para_blocos))

  (if (null lista_segmentos_para_blocos) 
    (progn (princ "\n[ERRO] Nenhum segmento válido para pontos médios.") (exit))
  )
  (princ 
    (strcat "\n" 
            (itoa (length lista_segmentos_para_blocos))
            " pontos médios calculados."
    )
  )

  (setq vla_layer_alvo (IAN:cbpm-ensure-layer layer_alvo_nome 7))
  (if (not vla_layer_alvo) (setq layer_alvo_nome old_clayer))

  (princ 
    (strcat "\nInserindo blocos '" nome_bloco_real "' na layer '" layer_alvo_nome 
            "'..."
    )
  )
  (foreach segmento lista_segmentos_para_blocos 
    (setq p_medio_calc (car segmento)
          p_orig_A     (cadr segmento)
          p_orig_B     (caddr segmento)
    )
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
        (if (not first_block_vla_obj) 
          (setq first_block_vla_obj vla_bloco_inserido_raw)
        )
        (if vla_layer_alvo (vla-put-Layer vla_bloco_inserido_raw layer_alvo_nome))
        (princ ".")
      )
      (princ 
        (strcat "\n  [ERRO] Falha ao inserir bloco '" 
                nome_bloco_real
                "' em "
                (vl-princ-to-string p_medio_calc)
        )
      )
    )
  )

  (if (> contador_blocos_inseridos 0) 
    (progn 
      (princ 
        (strcat "\nSucesso! " 
                (itoa contador_blocos_inseridos)
                " bloco(s) '"
                nome_bloco_real
                "' inserido(s)."
        )
      )
      first_block_vla_obj
    )
    (progn (princ "\nNenhum bloco foi inserido.") nil)
  )
)

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

;;; --- Comando Principal: COLOCAR_BLOCO_EM_PONTOS_MEDIOS (Interface com o Usuário) ---
(defun c:COLOCAR_BLOCO_EM_PONTOS_MEDIOS (/ *error* old_osmode arquivo_txt 
                                         ent_sel_data nome_bloco_real vla_bloco_modelo
                                        ) 
  (defun *error* (msg) 
    (IAN:std-error msg (function (lambda () (IAN:restore-osnap old_osmode))))
  )
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> INSERIR BLOCOS EM PONTOS MÉDIOS (CBPM) <<<")

  (setq arquivo_txt (getfiled "Selecione o arquivo TXT com coordenadas (X,Y,Z)" 
                              ""
                              "txt"
                              0
                    )
  )
  (if (and arquivo_txt (findfile arquivo_txt)) 
    (progn 
      (setq nome_bloco_real nil)
      (while (not nome_bloco_real) 
        (setq ent_sel_data (entsel "\nSelecione o bloco modelo a ser inserido: "))
        (if ent_sel_data 
          (if 
            (= "AcDbBlockReference" 
               (vla-get-ObjectName 
                 (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data)))
               )
            )
            (setq nome_bloco_real (vla-get-EffectiveName vla_bloco_modelo))
            (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
          )
          (setq nome_bloco_real "cancel") ; Sai do loop se o usuário pressionar ESC
        )
      )

      (if (not (equal nome_bloco_real "cancel")) 
        (progn 
          (princ (strcat "\nBloco selecionado: '" nome_bloco_real "'."))
          ;; Chama a função "Motor" com os dados do usuário
          (IAN:cbpm-core arquivo_txt nome_bloco_real)
        )
        (princ "\nSeleção de bloco cancelada.")
      )
    )
    (princ "\nArquivo não selecionado ou não encontrado. Comando cancelado.")
  )

  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:CBPM () (c:COLOCAR_BLOCO_EM_PONTOS_MEDIOS)) 


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;   COMANDO: IAN_RotateThenOffsetBlock (RTOB)                            ;;;
;;;   (Rotaciona no Ponto de Inserção e Desloca Perpendicularmente)        ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;
;;; Descrição: Rotaciona blocos selecionados em 180 graus em torno de seu  ;;;
;;;            ponto de inserção (eixo Z). Em seguida, desloca cada bloco  ;;;
;;;            uma distância especificada, 90 graus em relação à nova     ;;;
;;;            orientação do bloco.                                        ;;;
;;;-----------------------------------------------------------------------------



;;; --- Função "Motor" para RTGG: A lógica pura, sem interação ---
(defun IAN:rtgg-core (ss_blocks offset_distance / total_selected i ent_name vla_block 
                      current_insertion_point_vla current_insertion_point_list 
                      current_rotation_rad new_block_rotation_rad offset_angle_rad 
                      new_ins_x new_ins_y new_ins_z new_insertion_point_list 
                      processed_count error_obj
                     ) 
  (setq processed_count 0)
  (setq total_selected (sslength ss_blocks))
  (princ 
    (strcat "\nProcessando " 
            (itoa total_selected)
            " bloco(s) com offset de "
            (rtos offset_distance)
            "..."
    )
  )

  (setq i 0)
  (while (< i total_selected) 
    (setq ent_name (ssname ss_blocks i))
    (setq vla_block (vl-catch-all-apply 'vlax-ename->vla-object (list ent_name)))

    (if (not (vl-catch-all-error-p vla_block)) 
      (progn 
        (setq vla_block vla_block) ; Dereference
        (setq current_insertion_point_vla (vla-get-InsertionPoint vla_block))
        (setq current_rotation_rad (vla-get-Rotation vla_block))
        (setq current_insertion_point_list (vlax-safearray->list (vlax-variant-value current_insertion_point_vla)))

        ;; Rotacionar
        (setq new_block_rotation_rad (+ current_rotation_rad PI))
        (vla-put-Rotation vla_block new_block_rotation_rad)

        ;; Calcular novo ponto
        (setq offset_angle_rad (- new_block_rotation_rad (/ PI 2.0)))
        (setq new_ins_x (+ (car current_insertion_point_list) 
                           (* offset_distance (cos offset_angle_rad))
                        )
        )
        (setq new_ins_y (+ (cadr current_insertion_point_list) 
                           (* offset_distance (sin offset_angle_rad))
                        )
        )
        (setq new_ins_z (caddr current_insertion_point_list))
        (setq new_insertion_point_list (list new_ins_x new_ins_y new_ins_z))

        ;; Mover
        (vla-put-InsertionPoint vla_block (vlax-3d-point new_insertion_point_list))
        (setq processed_count (1+ processed_count))
      )
      (princ 
        (strcat "\n  AVISO: Falha ao obter objeto VLA. Erro: " 
                (vl-catch-all-error-message vla_block)
        )
      )
    )
    (setq i (1+ i))
  )
  processed_count ; Retorna o número de blocos processados
)

(defun c:RotacionarGeogrelhas (/ *error* s:old_cmdecho s:old_osmode offset_distance 
                               ss_blocks processed_count
                              ) 
  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg) 
    (IAN:std-error 
      msg
      (function 
        (lambda () 
          (if s:old_cmdecho (setvar "CMDECHO" s:old_cmdecho))
          (IAN:restore-osnap s:old_osmode)
        )
      )
    )
  )

  ;; --- Inicialização ---
  (princ "\n>>> RTGG - Rotacionar e Deslocar Blocos <<<")
  (setq s:old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq s:old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; 1. Pedir Parâmetros ao Usuário
  (initget 1) ; Não pode ser nulo
  (setq offset_distance (getdist "\nDigite a distância para o deslocamento perpendicular (ex: 8.0): "))
  (if (null offset_distance) 
    (progn (princ "\nDistância não fornecida. Comando cancelado.") (exit))
  )

  (princ "\nSelecione os blocos para processar:")
  (setq ss_blocks (ssget '((0 . "INSERT"))))
  (if (null ss_blocks) 
    (progn (princ "\nNenhum bloco selecionado. Comando cancelado.") (exit))
  )

  ;; 2. Chamar o Motor com os Dados Coletados
  (setq processed_count (IAN:rtgg-core ss_blocks offset_distance))

  ;; 3. Feedback Final
  (princ "\n-----------------------------------------------------")
  (if (> processed_count 0) 
    (princ 
      (strcat "\nSucesso! " 
              (itoa processed_count)
              " de "
              (itoa (sslength ss_blocks))
              " bloco(s) foram processados."
      )
    )
    (princ 
      (strcat "\nNenhum bloco foi processado com sucesso de " 
              (itoa (sslength ss_blocks))
              " selecionado(s)."
      )
    )
  )
  (princ "\n-----------------------------------------------------")

  ;; --- Limpeza Final ---
  (if s:old_cmdecho (setvar "CMDECHO" s:old_cmdecho))
  (IAN:restore-osnap s:old_osmode)
  (princ)
)
;; Alias para o comando
(defun c:RTGG () (c:RotacionarGeogrelhas))


;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;   COMANDO: CRIAR CAMADAS DE BLOCO EM ESCADINHA (CBE)                   ;;;
;;;                                                                        ;;;
;;;; ----------------------------------------------------------------------;;;;
;;; Descrição: A partir de uma seleção de blocos da camada base, cria      ;;;
;;;            camadas subsequentes de blocos em formato de "escadinha",   ;;;
;;;            com offsets horizontais e verticais definidos pelo usuário. ;;;
;;;-----------------------------------------------------------------------------

;;; --- Função Auxiliar para CBE: Lógica do Modo ZIG-ZAG (MODIFICADA) ---
(defun IAN:cbe-executar-zigzag (blocos_na_base blocos_deslocados mspace lista_linhas_perfil offset_h alt_v / 
                                direcao_escada_str direcao_sinal_z total_blocos_movidos total_blocos_criados 
                                bloco_a_mover pt_atual_bloco rot_bloco angulo_direcao_offset_h offset_vetor_unit_x 
                                offset_vetor_unit_y novo_ponto_movido lista_unificada vla_bloco_base pt_base_list 
                                rot_base nome_base layer_bloco_original z_atual_bloco delta_z_total z_final_local
                                qnt_novas_camadas blocos_criados_nesta_base n offset_h_acum novo_x novo_y novo_z 
                                ponto_insercao_nova_camada vla_bloco_copiado
                               ) 
  (princ "\nExecutando modo ZIG-ZAG...")
  
  (setq total_blocos_movidos 0 total_blocos_criados 0)

  ;; Ajuste do recuo (sem alterações aqui)
  (if blocos_deslocados 
    (progn 
      (princ (strcat "\nProcessando " (itoa (length blocos_deslocados)) " bloco(s) deslocado(s) (aplicando recuo horizontal)..."))
      (foreach bloco_a_mover blocos_deslocados 
        (setq pt_atual_bloco (IAN:abs-get-insertion-point-list bloco_a_mover))
        (setq rot_bloco (vla-get-Rotation bloco_a_mover))
        (setq angulo_direcao_offset_h (+ rot_bloco (/ PI 2.0)))
        (setq offset_vetor_unit_x (cos angulo_direcao_offset_h))
        (setq offset_vetor_unit_y (sin angulo_direcao_offset_h))
        (setq novo_ponto_movido (list (+ (car pt_atual_bloco) (* offset_h offset_vetor_unit_x)) (+ (cadr pt_atual_bloco) (* offset_h offset_vetor_unit_y)) (caddr pt_atual_bloco)))
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-InsertionPoint (list bloco_a_mover (vlax-3d-point novo_ponto_movido)))))
          (setq total_blocos_movidos (1+ total_blocos_movidos))
        )
        (princ ".")
      )
    )
  )

  (setq lista_unificada (append blocos_na_base blocos_deslocados))
  (if lista_unificada 
    (progn 
      (princ (strcat "\nProcessando " (itoa (length lista_unificada)) " blocos totais na fundação (criando camadas subsequentes)..."))
      (foreach vla_bloco_base lista_unificada 
        (setq pt_base_list (IAN:abs-get-insertion-point-list vla_bloco_base))
        (setq rot_base (vla-get-Rotation vla_bloco_base))
        (setq nome_base (vla-get-EffectiveName vla_bloco_base))
        (setq layer_bloco_original (vla-get-Layer vla_bloco_base))
        (setq z_atual_bloco (caddr pt_base_list) blocos_criados_nesta_base 0 qnt_novas_camadas 0)

        ;; --- INÍCIO DA MUDANÇA ---
        (setq z_final_local (IAN:cbe-get-target-z-for-block vla_bloco_base lista_linhas_perfil))
        (if z_final_local
          (progn
            (setq delta_z_total (- z_final_local z_atual_bloco))
            (setq direcao_escada_str (if (>= delta_z_total 0) "Subir" "Descer"))
            (setq direcao_sinal_z (if (eq direcao_escada_str "Subir") 1.0 -1.0))
            (if (> (abs delta_z_total) (* (* 2.0 alt_v) 0.1))
              (setq qnt_novas_camadas (fix (/ (abs delta_z_total) (* 2.0 alt_v))))
            )
          )
          (princ (strcat "\n  [AVISO] Bloco (Handle: " (vla-get-Handle vla_bloco_base) ") não encontrou perfil de topo. Ignorando."))
        )
        ;; --- FIM DA MUDANÇA ---

        (if (> qnt_novas_camadas 0) 
          (progn 
            (setq angulo_direcao_offset_h (if (eq direcao_escada_str "Subir") (+ rot_base (/ PI 2.0)) (- rot_base (/ PI 2.0))))
            (setq offset_vetor_unit_x (cos angulo_direcao_offset_h))
            (setq offset_vetor_unit_y (sin angulo_direcao_offset_h))
            (setq n 1)
            (while (<= n qnt_novas_camadas) 
              (setq offset_h_acum (* n (* 2.0 offset_h)))
              (setq novo_x (+ (car pt_base_list) (* offset_h_acum offset_vetor_unit_x)))
              (setq novo_y (+ (cadr pt_base_list) (* offset_h_acum offset_vetor_unit_y)))
              (setq novo_z (+ z_atual_bloco (* n direcao_sinal_z (* 2.0 alt_v))))
              (setq ponto_insercao_nova_camada (list novo_x novo_y novo_z))
              (setq vla_bloco_copiado (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point ponto_insercao_nova_camada) nome_base 1.0 1.0 1.0 rot_base)))
              (if (not (vl-catch-all-error-p vla_bloco_copiado)) 
                (progn 
                  (vla-put-Layer vla_bloco_copiado layer_bloco_original)
                  (setq total_blocos_criados (1+ total_blocos_criados) blocos_criados_nesta_base (1+ blocos_criados_nesta_base))
                )
                (princ (strcat "\n    [CBE ERRO] Falha ao inserir bloco '" nome_base "' na camada " (itoa n) ". Erro: " (vl-catch-all-error-message vla_bloco_copiado)))
              )
              (setq n (1+ n))
            )
            (if (> blocos_criados_nesta_base 0) (princ (strcat " -> " (itoa blocos_criados_nesta_base) " blocos adicionados.")))
          )
        )
      )
    )
  )
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
    (setq circulo_temp (vla-AddCircle mspace pt_insercao 0.4))
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

;;; --- Função Auxiliar para CBE: Lógica do Modo Escadinha Simples (MODIFICADA) ---
(defun IAN:cbe-executar-escadinha-simples (blocos_na_base mspace lista_linhas_perfil offset_h alt_v / 
                                           bloco_base_process_list vla_bloco_base 
                                           pt_base_list rot_base nome_base 
                                           layer_bloco_original direcao_escada_str 
                                           direcao_sinal_z total_blocos_criados bloco_info 
                                           z_atual_bloco delta_z_total z_final_local
                                           qnt_novas_camadas blocos_criados_nesta_base 
                                           angulo_direcao_offset_h offset_vetor_unit_x 
                                           offset_vetor_unit_y n offset_h_acum novo_x 
                                           novo_y novo_z ponto_insercao_nova_camada 
                                           vla_bloco_copiado
                                          ) 
  (princ "\nModo: Escadinha Simples. Todos os blocos estão na base.")

  ;; Prepara a lista de blocos (sem alterações aqui)
  (setq bloco_base_process_list nil)
  (foreach vla_bloco_base blocos_na_base 
    (setq pt_base_list (IAN:abs-get-insertion-point-list vla_bloco_base))
    (setq rot_base (vla-get-Rotation vla_bloco_base))
    (setq nome_base (vla-get-EffectiveName vla_bloco_base))
    (setq layer_bloco_original (vla-get-Layer vla_bloco_base))
    (setq bloco_base_process_list (cons (list vla_bloco_base pt_base_list rot_base nome_base layer_bloco_original) bloco_base_process_list))
  )
  (setq bloco_base_process_list (reverse bloco_base_process_list))

  (setq total_blocos_criados 0)
  (princ "\nCriando camadas em escadinha...")
  (foreach bloco_info bloco_base_process_list 
    (setq vla_bloco_base (nth 0 bloco_info) pt_base_list (nth 1 bloco_info) rot_base (nth 2 bloco_info)
          nome_base (nth 3 bloco_info) layer_bloco_original (nth 4 bloco_info)
    )
    (setq z_atual_bloco (caddr pt_base_list) blocos_criados_nesta_base 0 qnt_novas_camadas 0)

    ;; --- INÍCIO DA MUDANÇA ---
    ;; Chama a função "GPS" para obter o Z-alvo LOCAL para este bloco
    (setq z_final_local (IAN:cbe-get-target-z-for-block vla_bloco_base lista_linhas_perfil))

    (if z_final_local
      (progn
        (setq delta_z_total (- z_final_local z_atual_bloco))
        (setq direcao_escada_str (if (>= delta_z_total 0) "Subir" "Descer"))
        (setq direcao_sinal_z (if (eq direcao_escada_str "Subir") 1.0 -1.0))
        (if (> (abs delta_z_total) (* alt_v 0.1))
          (setq qnt_novas_camadas (fix (/ (abs delta_z_total) alt_v)))
        )
      )
      (princ (strcat "\n  [AVISO] Bloco (Handle: " (vla-get-Handle vla_bloco_base) ") não encontrou perfil de topo. Ignorando."))
    )
    ;; --- FIM DA MUDANÇA ---

    (if (> qnt_novas_camadas 0) 
      (progn 
        (princ (strcat "\n  Processando bloco (Handle: " (vla-get-Handle vla_bloco_base) "), criando " (itoa qnt_novas_camadas) " camada(s)..."))
        (setq angulo_direcao_offset_h (if (eq direcao_escada_str "Subir") (+ rot_base (/ PI 2.0)) (- rot_base (/ PI 2.0))))
        (setq offset_vetor_unit_x (cos angulo_direcao_offset_h) offset_vetor_unit_y (sin angulo_direcao_offset_h))
        (setq n 1)
        (while (<= n qnt_novas_camadas) 
          (setq offset_h_acum (* n offset_h))
          (setq novo_x (+ (car pt_base_list) (* offset_h_acum offset_vetor_unit_x)))
          (setq novo_y (+ (cadr pt_base_list) (* offset_h_acum offset_vetor_unit_y)))
          (setq novo_z (+ z_atual_bloco (* n direcao_sinal_z alt_v)))
          (setq ponto_insercao_nova_camada (list novo_x novo_y novo_z))
          (setq vla_bloco_copiado (vl-catch-all-apply 'vla-InsertBlock (list mspace (vlax-3d-point ponto_insercao_nova_camada) nome_base 1.0 1.0 1.0 rot_base)))
          (if (not (vl-catch-all-error-p vla_bloco_copiado)) 
            (progn
                   (vla-put-Layer vla_bloco_copiado layer_bloco_original)
                   (setq total_blocos_criados (1+ total_blocos_criados) blocos_criados_nesta_base (1+ blocos_criados_nesta_base))
                   (princ ".")
            )
            (princ (strcat "\n    [CBE ERRO] Falha ao inserir bloco '" nome_base "' na camada " (itoa n) ". Erro: " (vl-catch-all-error-message vla_bloco_copiado)))
          )
          (setq n (1+ n))
        )
        (if (> blocos_criados_nesta_base 0) (princ (strcat " " (itoa blocos_criados_nesta_base) " blocos adicionados.")))
      )
    )
  )
  (if (> total_blocos_criados 0) 
    (princ (strcat "\nSucesso! " (itoa total_blocos_criados) " novo(s) bloco(s) criado(s)."))
    (princ "\nNenhum novo bloco foi criado.")
  )
)

;;; --- FUNÇÃO DE CONVERSÃO SEGURA (ESSENCIAL) ---
;;; Recebe um dado e retorna sempre uma Lista de Ponto, não importa o que o AutoCAD devolver.
(defun IAN:safely-get-point-list (raw_point_data / data_type)
  (setq data_type (type raw_point_data))
  (cond
    ((= data_type 'VARIANT)
     (vlax-safearray->list (vlax-variant-value raw_point_data))
    )
    ((= data_type 'LIST)
     raw_point_data ; Já é uma lista, só a retorna.
    )
    (t nil) ; Se for qualquer outra coisa, retorna nil.
  )
)

;;; --- NOVA FUNÇÃO "GPS" (v8 - USANDO CONVERSÃO SEGURA) ---
(defun IAN:cbe-get-target-z-for-block (vla_block lista_linhas_perfil /
                                       pt_base rot_base perp_angle p1_wall p2_wall p1_wall_2d p2_wall_2d
                                       linha_topo pt_start_topo pt_end_topo pt_start_topo_2d pt_end_topo_2d
                                       intersections_list pt_inter_2d min_dist best_point dist z_alvo
                                      )
  ;; Função auxiliar interna para converter um ponto 3D para 2D
  (defun 2d (p) (list (car p) (cadr p)))
  
  (setq pt_base (IAN:abs-get-insertion-point-list vla_block))
  (setq rot_base (vla-get-Rotation vla_block))

  (setq perp_angle (+ rot_base (/ PI 2.0)))
  (setq p1_wall (polar pt_base perp_angle 1.0e9))
  (setq p2_wall (polar pt_base perp_angle -1.0e9))
  (setq p1_wall_2d (2d p1_wall))
  (setq p2_wall_2d (2d p2_wall))
  
  (setq intersections_list nil)

  ;; 1. Coleta TODAS as interseções válidas
  (foreach linha_topo lista_linhas_perfil
    ;; USA A FUNÇÃO DE CONVERSÃO SEGURA para evitar o erro de tipo
    (setq pt_start_topo (IAN:safely-get-point-list (vlax-curve-getStartPoint linha_topo)))
    (setq pt_end_topo (IAN:safely-get-point-list (vlax-curve-getEndPoint linha_topo)))
    
    (if (and pt_start_topo pt_end_topo)
      (progn
        (setq pt_start_topo_2d (2d pt_start_topo))
        (setq pt_end_topo_2d (2d pt_end_topo))
        (setq pt_inter_2d (inters p1_wall_2d p2_wall_2d pt_start_topo_2d pt_end_topo_2d T))
        
        (if pt_inter_2d
          (setq intersections_list (cons (list pt_inter_2d linha_topo) intersections_list))
        )
      )
    )
  )

  ;; 2. Se encontrou interseções, agora procura a MAIS PRÓXIMA do bloco
  (if intersections_list
    (progn
      (setq min_dist nil best_point nil)
      (foreach item intersections_list
        (setq pt_inter_2d (car item))
        (setq dist (distance (2d pt_base) pt_inter_2d))
        (if (or (null min_dist) (< dist min_dist))
          (progn
            (setq min_dist dist)
            (setq best_point item)
          )
        )
      )
      
      ;; 3. Com a melhor interseção (a mais próxima), finalmente calcula o Z
      (if best_point
        (setq z_alvo (caddr (vlax-curve-getClosestPointTo (cadr best_point) (append (car best_point) '(0.0)))))
      )
    )
  )
  z_alvo ; Retorna o Z encontrado ou nil
)


;;; --- Função "Motor" para CBE (MODIFICADA) ---
(defun IAN:cbe-core (ss_entidades_base ss_linhas_perfil offset_h alt_v / doc mspace 
                     i ent_name vla_ent lista_blocos lista_linhas_alinhamento 
                     lista_linhas_perfil_vla listas_classificadas blocos_na_base 
                     blocos_deslocados
                    ) 
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace (vla-get-ModelSpace doc))

  ;; --- 1. Separa as entidades da PRIMEIRA seleção ---
  (setq lista_blocos nil lista_linhas_alinhamento nil i 0)
  (while (< i (sslength ss_entidades_base))
    (setq ent_name (ssname ss_entidades_base i))
    (setq vla_ent (vlax-ename->vla-object ent_name))
    (if (= (vla-get-ObjectName vla_ent) "AcDbBlockReference")
      (setq lista_blocos (cons vla_ent lista_blocos))
      (setq lista_linhas_alinhamento (cons vla_ent lista_linhas_alinhamento))
    )
    (setq i (1+ i))
  )
  
  ;; --- 2. Converte a SEGUNDA seleção para uma lista de objetos VLA ---
  (setq lista_linhas_perfil_vla nil i 0)
  (while (< i (sslength ss_linhas_perfil))
    (setq vla_ent (vlax-ename->vla-object (ssname ss_linhas_perfil i)))
    (setq lista_linhas_perfil_vla (cons vla_ent lista_linhas_perfil_vla))
    (setq i (1+ i))
  )

  ;; --- Validação ---
  (if (or (null lista_blocos) (null lista_linhas_alinhamento) (null lista_linhas_perfil_vla))
    (progn (princ "\n[ERRO] As seleções estão incompletas. Verifique se selecionou os tipos certos de entidade.") (exit))
  )

  (princ (strcat "\nEntidades da Base: " (itoa (length lista_blocos)) " bloco(s) e " (itoa (length lista_linhas_alinhamento)) " linha(s) de alinhamento."))
  (princ (strcat "\nEntidades do Topo: " (itoa (length lista_linhas_perfil_vla)) " linha(s) de perfil."))

  ;; --- Classificar Blocos ---
  (setq listas_classificadas (IAN:cbe-classificar-blocos lista_blocos lista_linhas_alinhamento mspace))
  (setq blocos_na_base (car listas_classificadas))
  (setq blocos_deslocados (cadr listas_classificadas))

  ;; --- Decidir o Modo de Execução (Dispatcher) ---
  (if (null blocos_deslocados) 
    (IAN:cbe-executar-escadinha-simples blocos_na_base mspace lista_linhas_perfil_vla offset_h alt_v)
    (IAN:cbe-executar-zigzag blocos_na_base blocos_deslocados mspace lista_linhas_perfil_vla offset_h alt_v)
  )
  (princ)
)

;;;
;;; c:CRIAR_CAMADAS_BLOCO_ESCADINHA (CBE) - (MODIFICADA)
;;;
(defun c:CRIAR_CAMADAS_BLOCO_ESCADINHA (/ *error* old_cmdecho old_clayer old_osmode 
                                        ss_entidades_base ss_linhas_perfil
                                        offset_h alt_v
                                       ) 
  (defun *error* (msg) 
    (IAN:std-error msg (lambda () 
                         (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
                         (if old_clayer (setvar "CLAYER" old_clayer))
                         (IAN:restore-osnap old_osmode)
                       ))
  )

  (setq old_cmdecho (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  (setq old_clayer (getvar "CLAYER"))
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> CRIAR CAMADAS DE BLOCO (Perfil Dinâmico) <<<")

  ;; --- ETAPA 1: SELEÇÃO DE BASE ---
  (princ "\nSelecione os BLOCOS BASE e suas LINHAS DE ALINHAMENTO:")
  (setq ss_entidades_base (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_base
    (progn
      ;; --- ETAPA 2: SELEÇÃO DE PERFIL ---
      (princ "\nAgora, selecione as LINHAS DE PERFIL do topo:")
      (setq ss_linhas_perfil (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if ss_linhas_perfil
        (progn
          (initget 7)
          (setq offset_h (getdist "\nDigite o offset horizontal por camada (recuo/avanço, pode ser 0): "))
          (setq alt_v (getdist "\nDigite a altura vertical por camada: "))

          (if (and (numberp offset_h) alt_v (> alt_v 0.0))
            (IAN:cbe-core ss_entidades_base ss_linhas_perfil offset_h alt_v)
            (princ "\nParâmetros inválidos ou cancelados. Comando encerrado.")
          )
        )
        (princ "\nNenhuma linha de perfil selecionada. Comando cancelado.")
      )
    )
    (princ "\nNenhuma entidade de base selecionada. Comando cancelado.")
  )

  (setvar "CMDECHO" old_cmdecho)
  (setvar "CLAYER" old_clayer)
  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:CBE () (c:CRIAR_CAMADAS_BLOCO_ESCADINHA)) 


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
                                         current_block_obj blocos_restantes candidates 
                                         pt curpt next min_dist candpt dist
                                        ) 
  (setq ordered_list nil)
  (setq current_block_obj first_block_vla)
  (setq blocos_restantes (append candidate_blocks_vla nil))
  (while current_block_obj 
    (setq ordered_list (cons current_block_obj ordered_list))
    (setq blocos_restantes (vl-remove-if 
                             (function 
                               (lambda (blk) 
                                 (equal (vla-get-Handle blk) 
                                        (vla-get-Handle current_block_obj)
                                 )
                               )
                             )
                             blocos_restantes
                           )
    )
    (if blocos_restantes 
      (progn 
        (setq curpt (cdr (assoc (vla-get-Handle current_block_obj) block_point_map)))
        (setq next     nil
              min_dist nil
        )
        (if curpt 
          (foreach cand blocos_restantes 
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



;;; --- Função "Motor" para ALTBLOCOSEQ: A lógica pura, sem interação ---
(defun IAN:abs-core (first_block_vla height_offset / first_block_name 
                     all_instances_of_block point_map ordered_block_sequence 
                     block_vla_in_loop current_insertion_pt_list new_insertion_pt_list 
                     vla_operation_error_obj modified_block_count idx next_index 
                     last_displaced z_anterior dois_vezes_offset tol z_atual delta 
                     deltaA quoc divisivel?
                    ) 
  (setq modified_block_count 0)
  (setq first_block_name (vla-get-EffectiveName first_block_vla))

  ;; Coleta instâncias e ordena
  (setq all_instances_of_block (IAN:abs-get-block-instances first_block_name))
  (if (< (length all_instances_of_block) 1) 
    (progn 
      (princ 
        (strcat "\nNenhuma instância do bloco '" first_block_name "' encontrada.")
      )
      (return 0)
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

  (if (< (length ordered_block_sequence) 2) 
    (progn (princ "\nNão foi possível ordenar uma sequência de pelo menos 2 blocos.") 
           (return 0)
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
  (setq idx            0
        next_index     0
        last_displaced nil
        z_anterior     nil
  )
  (setq dois_vezes_offset (* 2.0 height_offset)
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
            (setq delta      (- z_atual z_anterior)
                  deltaA     (abs delta)
                  quoc       (/ deltaA dois_vezes_offset)
                  divisivel? (equal (- quoc (fix quoc)) 0.0 tol)
            )
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
  ordered_block_sequence ; Retorna a lista de VLA objects da sequência ordenada
)

;; Comando: altera altura Z de blocos em sequência (um sim, um não)
(defun c:ALTBLOCOSEQ (/ *error* old_cmdecho old_osmode first_block_ename 
                      first_block_vla vla_operation_error_obj height_offset 
                      modified_block_count
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

  (princ "\n>>> ALTERAR ALTURA DE BLOCOS EM SEQUÊNCIA (ALTBLOCOSEQ) <<<")

  ;; Etapa 1: Selecionar o bloco inicial
  (setq first_block_vla nil)
  (while (not first_block_vla) 
    (setq first_block_ename (car (entsel "\nSelecione o PRIMEIRO bloco da sequência: ")))
    (cond 
      ((null first_block_ename)
       (princ "\nSeleção cancelada.")
       (exit)
      )
      ((vl-catch-all-error-p 
         (setq vla_operation_error_obj (vl-catch-all-apply 'vlax-ename->vla-object 
                                                           (list first_block_ename)
                                       )
         )
       )
       (princ 
         (strcat "\n[ABS ERRO] Falha ao obter objeto VLA: " 
                 (vl-catch-all-error-message vla_operation_error_obj)
         )
       )
      )
      ((not 
         (and 
           (vlax-property-available-p (setq first_block_vla vla_operation_error_obj) 
                                      'ObjectName
           )
           (= (vla-get-ObjectName first_block_vla) "AcDbBlockReference")
         )
       )
       (princ "\nA entidade selecionada não é um bloco. Tente novamente.")
       (setq first_block_vla nil)
      )
    )
  )
  (princ 
    (strcat "\nBloco inicial selecionado: '" 
            (vla-get-EffectiveName first_block_vla)
            "'."
    )
  )

  ;; Etapa 2: Obter altura do usuário
  (setq height_offset (getreal "\nDigite a altura de deslocamento Z para blocos alternados (ex: 0.5): "))
  (if (or (null height_offset) (equal height_offset 0.0 1e-8)) 
    (progn (princ "\nAltura de deslocamento inválida ou zero. Comando cancelado.") 
           (exit)
    )
  )

  ;; Etapa 3: Chamar o motor com os dados coletados
  (setq lista_blocos_modificados (IAN:abs-core first_block_vla height_offset))

  ;; Etapa 4: Feedback Final
  (if (and lista_blocos_modificados (> (length lista_blocos_modificados) 0)) 
    (princ 
      (strcat "\nSucesso! " 
              (itoa (length lista_blocos_modificados))
              " blocos na sequência processados."
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



;;; --- Função "Motor" para DELBLOCINV: A lógica pura, sem interação ---
(defun IAN:delblocinv-core (first_block_vla height_offset / first_block_name 
                            all_instances_of_block point_map ordered_block_sequence 
                            deleted_block_count idx next_index last_deleted z_anterior 
                            dois_vezes_offset tol block_vla_in_loop current_pt z_atual 
                            delta divisivel? vla_operation_error_obj
                           ) 
  (setq deleted_block_count 0)
  (setq first_block_name (vla-get-EffectiveName first_block_vla))

  ;; Coleta instâncias e ordena
  (setq all_instances_of_block (IAN:abs-get-block-instances first_block_name))
  (if (< (length all_instances_of_block) 1) 
    (progn 
      (princ 
        (strcat "\nNenhuma instância do bloco '" first_block_name "' encontrada.")
      )
      (return 0)
    )
  )
  (princ 
    (strcat "\nEncontradas " 
            (itoa (length all_instances_of_block))
            " instâncias totais."
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

  (if (null ordered_block_sequence) 
    (progn (princ "\nNão foi possível ordenar a sequência de blocos.") (return 0))
  )
  (princ 
    (strcat "\n" 
            (itoa (length ordered_block_sequence))
            " blocos ordenados na sequência."
    )
  )

  ;; Remoção intercalada adaptativa
  (princ "\nRemovendo blocos com lógica adaptativa...")
  (setq idx          0
        next_index   0
        last_deleted nil
        z_anterior   nil
  )
  (setq dois_vezes_offset (* 2.0 height_offset)
        tol               1e-8
  )

  (foreach block_vla_in_loop ordered_block_sequence 
    (setq current_pt (cdr (assoc (vla-get-Handle block_vla_in_loop) point_map)))
    (if current_pt 
      (progn 
        (setq z_atual (caddr current_pt))
        (if (and z_anterior (not (equal z_atual z_anterior tol))) 
          (progn 
            (setq delta      (- z_atual z_anterior)
                  divisivel? (equal 
                               (- (/ (abs delta) dois_vezes_offset) 
                                  (fix (/ (abs delta) dois_vezes_offset))
                               )
                               0.0
                               tol
                             )
            )
            (cond 
              ((and divisivel? last_deleted) (setq next_index 1))
              ((and divisivel? (not last_deleted)) (setq next_index 0))
              ((and (not divisivel?) last_deleted) (setq next_index 0))
              ((and (not divisivel?) (not last_deleted)) (setq next_index 1))
            )
          )
        )
        (if (= next_index 0) 
          (progn 
            (setq vla_operation_error_obj (vl-catch-all-apply 'vla-Delete 
                                                              (list block_vla_in_loop)
                                          )
            )
            (if (not (vl-catch-all-error-p vla_operation_error_obj)) 
              (setq deleted_block_count (1+ deleted_block_count))
              (princ 
                (strcat "\n  [AVISO] Falha ao apagar bloco: " 
                        (vl-catch-all-error-message vla_operation_error_obj)
                )
              )
            )
            (setq last_deleted T)
          )
          (setq last_deleted nil)
        )
        (setq next_index (- 1 next_index))
        (setq z_anterior z_atual)
      )
      (princ 
        (strcat "\n  [AVISO] Falha ao obter ponto do bloco (Handle: " 
                (vla-get-Handle block_vla_in_loop)
                ")."
        )
      )
    )
    (setq idx (1+ idx))
  )
  deleted_block_count ; Retorna o número de blocos apagados
)

;;;
;;; c:APAGAR_BLOCOS_INTERCALADOS_INVERSO (DELBLOCINV)
;;;
(defun c:APAGAR_BLOCOS_INTERCALADOS_INVERSO (/ *error* old_cmdecho old_osmode 
                                             first_block_ename first_block_vla 
                                             vla_operation_error_obj height_offset 
                                             deleted_block_count
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
  (setvar "CMDECHO" 0)
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> APAGAR BLOCOS INTERCALADOS EM SEQUÊNCIA (ORDEM INVERSA) <<<")

  ;; --- Etapa 1: Selecionar o Primeiro Bloco ---
  (setq first_block_vla nil)
  (while (not first_block_vla) 
    (setq first_block_ename (car (entsel "\nSelecione o PRIMEIRO bloco da sequência para iniciar a remoção: ")))
    (cond 
      ((null first_block_ename)
       (princ "\nSeleção cancelada.")
       (exit)
      )
      ((vl-catch-all-error-p 
         (setq vla_operation_error_obj (vl-catch-all-apply 'vlax-ename->vla-object 
                                                           (list first_block_ename)
                                       )
         )
       )
       (princ 
         (strcat "\n[ERRO] Falha ao obter objeto VLA: " 
                 (vl-catch-all-error-message vla_operation_error_obj)
         )
       )
      )
      ((not 
         (and 
           (vlax-property-available-p (setq first_block_vla vla_operation_error_obj) 
                                      'ObjectName
           )
           (= (vla-get-ObjectName first_block_vla) "AcDbBlockReference")
         )
       )
       (princ "\nA entidade selecionada não é um bloco. Tente novamente.")
       (setq first_block_vla nil)
      )
    )
  )
  (princ 
    (strcat "\nBloco inicial selecionado: '" 
            (vla-get-EffectiveName first_block_vla)
            "'."
    )
  )

  ;; --- Etapa 2: Obter Altura de Referência ---
  (setq height_offset (getreal "\nInforme o mesmo deslocamento Z usado na criação (ex: 0.75): "))
  (if (or (null height_offset) (<= (abs height_offset) 1e-8)) 
    (progn (princ "\nValor inválido. Comando cancelado.") (exit))
  )

  ;; --- Etapa 3: Chamar o Motor ---
  (setq deleted_block_count (IAN:delblocinv-core first_block_vla height_offset))

  ;; --- Etapa 4: Feedback Final ---
  (if (> deleted_block_count 0) 
    (princ 
      (strcat "\nSucesso! " (itoa deleted_block_count) " bloco(s) foram apagados.")
    )
    (princ "\nNenhum bloco foi apagado (verifique a sequência e a altura).")
  )

  ;; --- Finalização ---
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)
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

;;; --- Função de Desenho para o Menu Gráfico ---
(defun IAN:desenhar-menu-grafico (titulo opcoes p1 p2 p3 p4 item_height item_realcado 
                                  texto_size / entidades_menu i start_y texto_y 
                                  texto_x cor
                                 ) 

  (setq entidades_menu nil)

  ;; --- 1. CRIA O FUNDO SÓLIDO PRIMEIRO ---
  (entmake 
    (list '(0 . "SOLID") 
          (cons 10 p1) ; Canto Superior Esquerdo
          (cons 11 p2) ; Canto Superior Direito
          (cons 12 p4) ; Canto Inferior Esquerdo
          (cons 13 p3) ; Canto Inferior Direito
          '(62 . 8) ; Cor (Cinza Escuro)
    )
  )
  (setq entidades_menu (cons (entlast) entidades_menu))


  ;; --- 2. DESENHA A BORDA E O REALCE TEMPORÁRIO ---
  (grdraw p1 p2 7)
  (grdraw p2 p3 7)
  (grdraw p3 p4 7)
  (grdraw p4 p1 7)
  (if (and item_realcado (>= item_realcado 0)) 
    (progn 
      (setq start_y (- (cadr p1) (* item_height 1.5)))
      (setq texto_y (- start_y (* item_realcado item_height)))
      ;; Usa uma cor brilhante para o realce, como amarelo
      (grdraw (list (car p1) (+ texto_y item_height)) 
              (list (car p2) (+ texto_y item_height))
              2
      )
      (grdraw (list (car p1) texto_y) (list (car p2) texto_y) 2)
    )
  )

  ;; --- 3. CRIA ENTIDADES DE TEXTO REAIS SOBRE O FUNDO ---

  ;; Cria a entidade de Título
  (entmake 
    (list '(0 . "TEXT") 
          (cons 10 (list (car p1) (+ (cadr p1) (* item_height 0.3)) 0))
          (cons 40 texto_size)
          (cons 1 (strcat " " titulo))
          '(7 . "STANDARD") ; Estilo de texto
          '(62 . 6) ; Cor (Magenta)
    )
  )
  (setq entidades_menu (cons (entlast) entidades_menu))

  ;; Cria as entidades das Opções
  (setq i 0)
  (setq start_y (- (cadr p1) (* item_height 1.5)))
  (setq texto_x (+ (car p1) (* item_height 0.5)))
  (foreach opcao opcoes 
    (setq texto_y (- start_y (* i item_height)))
    ;; O texto fica sempre branco, o fundo que muda (com grdraw)
    (setq cor 7)

    (entmake 
      (list '(0 . "TEXT") 
            (cons 10 (list texto_x texto_y 0))
            (cons 40 texto_size)
            (cons 1 (strcat " " opcao))
            '(7 . "STANDARD")
            (cons 62 cor)
      )
    )
    (setq entidades_menu (cons (entlast) entidades_menu))
    (setq i (1+ i))
  )

  (reverse entidades_menu) ; Retorna a lista de todas as entidades criadas (fundo + textos)
)


;;; --- Função Principal do Menu Gráfico ---
(defun IAN:menu-grafico (titulo opcoes / view_center view_height box_width box_height 
                         half_width half_height p1 p2 p3 p4 item_height start_y i 
                         texto_y texto_x looping item_selecionado ultimo_item_realcado 
                         mouse_data codigo_input ponto_mouse entidades_menu_atual
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

  (setq p1 (list (- (car view_center) half_width) 
                 (+ (cadr view_center) half_height)
           )
  )
  (setq p2 (list (+ (car view_center) half_width) 
                 (+ (cadr view_center) half_height)
           )
  )
  (setq p3 (list (+ (car view_center) half_width) 
                 (- (cadr view_center) half_height)
           )
  )
  (setq p4 (list (- (car view_center) half_width) 
                 (- (cadr view_center) half_height)
           )
  )

  ;; --- 2. Loop Principal de Interação ---
  (setq looping              T
        ultimo_item_realcado -1
        entidades_menu_atual nil
  )
  (while looping 
    ;; Apaga as entidades de texto antigas, se existirem
    (if entidades_menu_atual 
      (foreach ent entidades_menu_atual (if (entget ent) (entdel ent)))
    )
    (redraw) ; Limpa os gráficos temporários (realce)

    ;; *** CORREÇÃO AQUI: Adicionados p3 e p4 à chamada da função de desenho ***
    (setq entidades_menu_atual (IAN:desenhar-menu-grafico titulo opcoes p1 p2 p3 p4 
                                                          item_height ultimo_item_realcado texto_size
                               )
    )

    (setq mouse_data (grread T 15 0))
    (setq codigo_input (car mouse_data))
    (setq ponto_mouse (cadr mouse_data))

    (cond 
      ((= codigo_input 5) ; Mouse moveu
       (setq i                0
             item_selecionado -1
       )
       (setq start_y (- (cadr p1) (* item_height 1.5)))
       (while (and (< i (length opcoes)) (= item_selecionado -1)) 
         (setq texto_y (- start_y (* i item_height)))
         (if 
           (and ponto_mouse 
                (<= (car ponto_mouse) (car p2))
                (>= (car ponto_mouse) (car p1))
                (<= (cadr ponto_mouse) (+ texto_y item_height))
                (>= (cadr ponto_mouse) texto_y)
           )
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
       (setq looping          nil
             item_selecionado nil
       )
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
  (setq lista_de_tipos '("Terramesh" "Terra Armada" "Lock & Load" "Geogrelha" 
                         "StrataSlope" "Quadratum"
                        )
  )

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
        ((= escolha "StrataSlope")
         (c:CONSTRUIR_MURO_STRATASLOPE)
        )
        ((= escolha "Quadratum")
         (c:CONSTRUIR_MURO_QUADRATUM)
        )
      )
    )
    (princ "\nComando cancelado.")
  )
  (princ)
)
(defun c:CT () (c:CONTENCOES)) ; Alias curto


;;; ------------------------------------------------------------------------
;;; --- Funções de Construção Específicas (Placeholders) ---
;;; ------------------------------------------------------------------------

;; --- Lógica para construir um Muro Terramesh ---
(defun c:CONSTRUIR_MURO_TERRAMESH (/ ss_base_terramesh caminho_arquivo 
                                   nome_bloco_terramesh ent_sel_data vla_bloco_modelo 
                                   ss_entidades_subir ss_entidades_descer z_final
                                  ) 
  (princ "\n>>> INICIANDO CONSTRUCAO DE MURO TERRAMESH <<<")

  (princ "\nSelecione a linha de base para o muro Terramesh:")
  (setq ss_base_terramesh (ssget '((0 . "LINE,LWPOLYLINE"))))

  (if ss_base_terramesh 
    (progn 
      ;; =================================================================
      ;; --- PARTE 1: GERAR PONTOS (GPR) ---
      ;; =================================================================
      (setq caminho_arquivo (strcat (getvar "DWGPREFIX") "terramesh_pontos.txt"))
      (princ (strcat "\nArquivo de saida sera salvo em: " caminho_arquivo))

      (if (IAN:gerar-pontos-regua-core ss_base_terramesh 2.0 caminho_arquivo) 
        (progn 
          (princ "\nPontos do eixo Terramesh gerados com sucesso.")

          ;; =================================================================
          ;; --- PARTE 2: COLOCAR BLOCOS (CBPM) ---
          ;; =================================================================
          (setq nome_bloco_terramesh nil)
          (while (not nome_bloco_terramesh) 
            (setq ent_sel_data (entsel "\nSelecione o bloco modelo para o Terramesh: "))
            (if ent_sel_data 
              (if 
                (= "AcDbBlockReference" 
                   (vla-get-ObjectName 
                     (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data)))
                   )
                )
                (setq nome_bloco_terramesh (vla-get-EffectiveName vla_bloco_modelo))
                (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
              )
              (setq nome_bloco_terramesh "cancel")
            )
          )

          (if (not (equal nome_bloco_terramesh "cancel")) 
            (progn 
              (princ (strcat "\nBloco selecionado: '" nome_bloco_terramesh "'."))
              ;; Chama o "motor" do CBPM com o arquivo gerado e o bloco selecionado
              (IAN:cbpm-core caminho_arquivo nome_bloco_terramesh)
              (princ "\n\n--- Construcao de Muro Terramesh (Primeira Fiada) Concluida ---")
              ;; =================================================================
              ;; --- PARTE 3: CONSTRUIR CAMADAS SUPERIORES (CBE - SUBIR) ---
              ;; =================================================================
              (princ "\n\n--- Etapa de Subida ---")
              (princ "\nSelecione os blocos e as linhas de apoio para SUBIR (ou pressione ENTER para pular):")
              (setq ss_entidades_subir (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

              (if ss_entidades_subir 
                (progn 
                  (setq z_final (getreal "\nDigite a coordenada Z FINAL para a subida: "))
                  (if z_final 
                    (IAN:cbe-core ss_entidades_subir "Subir" 0.1 1.0 z_final)
                    (princ "\nZ Final não informado. Etapa pulada.")
                  )
                )
                (princ "\nNenhuma entidade selecionada para subir. Etapa pulada.")
              )

              ;; =================================================================
              ;; --- PARTE 4: CONSTRUIR CAMADAS INFERIORES (CBE - DESCER) ---
              ;; =================================================================
              (princ "\n\n--- Etapa de Descida ---")
              (princ "\nSelecione os blocos e as linhas de apoio para DESCER (ou pressione ENTER para pular):")
              (setq ss_entidades_descer (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

              (if ss_entidades_descer 
                (progn 
                  (setq z_final (getreal "\nDigite a coordenada Z FINAL para a descida: "))
                  (if z_final 
                    (IAN:cbe-core ss_entidades_descer "Descer" 0.1 1.0 z_final)
                    (princ "\nZ Final não informado. Etapa pulada.")
                  )
                )
                (princ "\nNenhuma entidade selecionada para descer. Etapa pulada.")
              )
            )
            (princ "\nSeleção de bloco cancelada.")
          )
        )
        (princ "\nFalha ao gerar pontos para o Terramesh.")
      )
    )
    (princ "\nNenhuma linha de base selecionada. Comando cancelado.")
  )

  (princ)
)

;; --- Lógica para construir um Muro Terra Armada ---
(defun c:CONSTRUIR_MURO_TERRA_ARMADA (/ *error* old_osmode old_cmdecho ss_base_ta 
                                      caminho_arquivo nome_bloco_inteiro ent_sel_data 
                                      vla_bloco_modelo first_block_created 
                                      all_base_blocks_vla z_final nome_bloco_meio
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
  (setvar "CMDECHO" 0)
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> INICIANDO CONSTRUCAO DE MURO TERRA ARMADA <<<")

  ;; =================================================================
  ;; --- FASE 1: BLOCO INTEIRO ---
  ;; =================================================================
  (princ "\n\n--- FASE 1: Construção com Bloco Inteiro ---")

  ;; --- Etapa 1.1: Gerar Pontos (GPR) ---
  (princ "\nSelecione a linha de base para o muro:")
  (setq ss_base_ta (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss_base_ta) 
    (progn (princ "\nLinha de base não selecionada. Comando cancelado.") (exit))
  )

  (setq caminho_arquivo (strcat (getvar "DWGPREFIX") "terra_armada_pontos.txt"))
  (princ (strcat "\nArquivo de pontos será salvo em: " caminho_arquivo))

  (if (not (IAN:gerar-pontos-regua-core ss_base_ta 1.5 caminho_arquivo)) 
    (progn (princ "\nFalha ao gerar pontos para o Terra Armada. Comando cancelado.") 
           (exit)
    )
  )
  (princ "\nPontos do eixo gerados com sucesso.")

  ;; --- Etapa 1.2: Colocar Blocos (CBPM) ---
  (setq nome_bloco_inteiro nil)
  (while (not nome_bloco_inteiro) 
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo INTEIRO: "))
    (if ent_sel_data 
      (if 
        (= "AcDbBlockReference" 
           (vla-get-ObjectName (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data))))
        )
        (setq nome_bloco_inteiro (vla-get-EffectiveName vla_bloco_modelo))
        (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
      )
      (progn (princ "\nSeleção de bloco cancelada.") (exit))
    )
  )
  (setq first_block_created (IAN:cbpm-core caminho_arquivo nome_bloco_inteiro))
  (princ "\n\n--- Primeira fiada de blocos inteiros criada. ---")

  ;; --- Etapa 1.3: Alternar Altura (ABS-Core) ---
  (if (not first_block_created) 
    (progn (princ "\nNenhum bloco foi criado na etapa anterior. Cancelando.") 
           (exit)
    )
  )
  (setq all_base_blocks_vla (IAN:abs-core first_block_created 0.75))
  (princ "\n\n--- Altura da primeira fiada alternada automaticamente. ---")

  ;; --- Etapa 1.4: Criar Camadas Verticais (CBE-Core) ---
  (if (not all_base_blocks_vla) 
    (progn (princ "\nNenhum bloco foi processado na etapa anterior. Cancelando.") 
           (exit)
    )
  )

  (princ "\nMontando seleção para construção vertical automaticamente...")

  ;; Cria um novo "carrinho de compras" (selection set) vazio
  (setq ss_entidades_cbe (ssadd))

  ;; Adiciona a linha de base original que o usuário selecionou no começo
  (ssadd (ssname ss_base_ta 0) ss_entidades_cbe)

  ;; Passa por cada bloco da lista que guardamos e adiciona no carrinho
  (foreach blk all_base_blocks_vla 
    (ssadd (vlax-vla-object->ename blk) ss_entidades_cbe)
  )

  (setq z_final (getreal "\nDigite a coordenada Z FINAL desejada para o muro: "))
  (if (not z_final) 
    (progn (princ "\nZ Final não informado. Comando cancelado.") (exit))
  )

  ;; Chama o CBE com a seleção que montamos sozinhos
  (IAN:cbe-core ss_entidades_cbe "Subir" 0.0 0.75 z_final)
  (princ "\n\n--- Construção vertical dos blocos inteiros concluída. ---")

  ;; =================================================================
  ;; --- FASE 2: MEIO BLOCO ---
  ;; =================================================================
  (princ "\n\n--- FASE 2: Inserção do Meio Bloco ---")

  ;; --- Etapa 2.1: Colocar Meios Blocos (CBPM) ---
  (setq nome_bloco_meio nil)
  (while (not nome_bloco_meio) 
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo MEIO BLOCO: "))
    (if ent_sel_data 
      (if 
        (= "AcDbBlockReference" 
           (vla-get-ObjectName (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data))))
        )
        (setq nome_bloco_meio (vla-get-EffectiveName vla_bloco_modelo))
        (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
      )
      (progn (princ "\nSeleção de bloco cancelada.") (exit))
    )
  )
  (setq first_block_created (IAN:cbpm-core caminho_arquivo nome_bloco_meio))
  (princ "\n\n--- Fiada de meios blocos criada. ---")

  ;; --- Etapa 2.2: Apagar Meios Blocos Intercalados (DELBLOCINV-Core) ---
  (if (not first_block_created) 
    (progn (princ "\nNenhum meio bloco foi criado na etapa anterior. Cancelando.") 
           (exit)
    )
  )

  (IAN:delblocinv-core first_block_created 0.75)
  (princ "\n\n--- Meios blocos intercalados apagados automaticamente. ---")

  ;; --- Finalização ---
  (princ "\n\n>>> Construção de Muro Terra Armada concluída! <<<")
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)

;; --- Lógica para construir um Muro Lock & Load ---
(defun c:CONSTRUIR_MURO_LOCKNLOAD (/ *error* old_osmode old_cmdecho ss_base_lnl 
                                   caminho_arquivo nome_bloco_lnl ent_sel_data 
                                   vla_bloco_modelo first_block_created 
                                   ss_entidades_subir ss_entidades_descer z_final
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
  (setvar "CMDECHO" 0)
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> INICIANDO CONSTRUCAO DE MURO LOCK & LOAD <<<")

  ;; --- Etapa 1: Gerar Pontos (GPR) com régua de 0.4 ---
  (princ "\nSelecione a linha de base para o muro:")
  (setq ss_base_lnl (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss_base_lnl) 
    (progn (princ "\nLinha de base não selecionada. Comando cancelado.") (exit))
  )

  (setq caminho_arquivo (strcat (getvar "DWGPREFIX") "locknload_pontos.txt"))
  (princ (strcat "\nArquivo de pontos será salvo em: " caminho_arquivo))

  (if (not (IAN:gerar-pontos-regua-core ss_base_lnl 0.4 caminho_arquivo)) 
    (progn (princ "\nFalha ao gerar pontos para o Lock & Load. Comando cancelado.") 
           (exit)
    )
  )
  (princ "\nPontos do eixo gerados com sucesso.")

  ;; --- Etapa 2: Colocar Blocos (CBPM) ---
  (setq nome_bloco_lnl nil)
  (while (not nome_bloco_lnl) 
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo LOCK & LOAD: "))
    (if ent_sel_data 
      (if 
        (= "AcDbBlockReference" 
           (vla-get-ObjectName (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data))))
        )
        (setq nome_bloco_lnl (vla-get-EffectiveName vla_bloco_modelo))
        (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
      )
      (progn (princ "\nSeleção de bloco cancelada.") (exit))
    )
  )
  (setq first_block_created (IAN:cbpm-core caminho_arquivo nome_bloco_lnl))
  (princ "\n\n--- Primeira fiada de blocos Lock & Load criada. ---")

  ;; --- Etapa 3: Alternar Altura (ABS-Core) com altura de 0.4 ---
  (if (not first_block_created) 
    (progn (princ "\nNenhum bloco foi criado na etapa anterior. Cancelando.") 
           (exit)
    )
  )
  (setq lista_blocos_processados (IAN:abs-core first_block_created 0.4))
  (if (not lista_blocos_processados) 
    (progn (princ "\nFalha ao processar a sequência de blocos para alternar a altura. Cancelando.") 
           (exit)
    )
  )
  (princ "\n\n--- Altura da primeira fiada alternada automaticamente. ---")

  ;; =================================================================
  ;; --- FASE 2: CONSTRUÇÃO VERTICAL (CBE) com altura e offset de 0.4 ---
  ;; =================================================================

  ;; --- Etapa de Subida ---
  (princ "\n\n--- Etapa de Subida ---")
  (princ "\nSelecione os blocos e linhas de apoio para SUBIR (ou pressione ENTER para pular):")
  (setq ss_entidades_subir (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_subir 
    (progn 
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a subida: "))
      (if z_final 
        (IAN:cbe-core ss_entidades_subir "Subir" 0.04 0.4 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para subir. Etapa pulada.")
  )

  ;; --- Etapa de Descida ---
  (princ "\n\n--- Etapa de Descida ---")
  (princ "\nSelecione os blocos e as linhas de apoio para DESCER (ou pressione ENTER para pular):")
  (setq ss_entidades_descer (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_descer 
    (progn 
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a descida: "))
      (if z_final 
        (IAN:cbe-core ss_entidades_descer "Descer" 0.04 0.4 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para descer. Etapa pulada.")
  )

  ;; --- Finalização ---
  (princ "\n\n>>> Construção de Muro Lock & Load concluída! <<<")
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)

;; --- Lógica para construir um Muro de Geogrelha ---
(defun c:CONSTRUIR_MURO_GEOGRELHA (/ *error* old_osmode old_cmdecho ss_base_gg 
                                   caminho_arquivo nome_bloco_gg ent_sel_data 
                                   vla_bloco_modelo ss_geogrelhas_para_inverter 
                                   ss_entidades_subir ss_entidades_descer z_final
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
  (setvar "CMDECHO" 0)
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> INICIANDO CONSTRUCAO DE MURO DE GEOGRELHA <<<")

  ;; --- Etapa 1: Gerar Pontos (GPR) ---
  (princ "\nSelecione a linha de base para o muro:")
  (setq ss_base_gg (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss_base_gg) 
    (progn (princ "\nLinha de base não selecionada. Comando cancelado.") (exit))
  )

  (setq caminho_arquivo (strcat (getvar "DWGPREFIX") "geogrelha_pontos.txt"))
  (princ (strcat "\nArquivo de pontos será salvo em: " caminho_arquivo))

  (if (not (IAN:gerar-pontos-regua-core ss_base_gg 4.9 caminho_arquivo)) 
    (progn (princ "\nFalha ao gerar pontos para a Geogrelha. Comando cancelado.") 
           (exit)
    )
  )
  (princ "\nPontos do eixo gerados com sucesso.")

  ;; --- Etapa 2: Colocar Blocos (CBPM) ---
  (setq nome_bloco_gg nil)
  (while (not nome_bloco_gg) 
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo da GEOGRELHA: "))
    (if ent_sel_data 
      (if 
        (= "AcDbBlockReference" 
           (vla-get-ObjectName (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data))))
        )
        (setq nome_bloco_gg (vla-get-EffectiveName vla_bloco_modelo))
        (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
      )
      (progn (princ "\nSeleção de bloco cancelada.") (exit))
    )
  )
  (IAN:cbpm-core caminho_arquivo nome_bloco_gg)
  (princ "\n\n--- Primeira fiada de blocos de geogrelha criada. ---")

  ;; --- Etapa 3: Inverter Geogrelhas (RTGG-Core) ---
  (princ "\nSelecione TODAS as geogrelhas da fiada para inverter:")
  (setq ss_geogrelhas_para_inverter (ssget '((0 . "INSERT"))))
  (if ss_geogrelhas_para_inverter 
    (progn 
      (princ "\nInvertendo geogrelhas com comprimento fixo de 8.0...")
      (IAN:rtgg-core ss_geogrelhas_para_inverter 8.0)
      (princ "\n\n--- Geogrelhas invertidas. ---")
    )
    (princ "\nNenhuma geogrelha selecionada para inverter. Etapa pulada.")
  )

  ;; =================================================================
  ;; --- FASE 2: CONSTRUÇÃO VERTICAL (CBE) ---
  ;; =================================================================

  ;; --- Etapa de Subida ---
  (princ "\n\n--- Etapa de Subida ---")
  (princ "\nSelecione os blocos e linhas de apoio para SUBIR (ou pressione ENTER para pular):")
  (setq ss_entidades_subir (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_subir 
    (progn 
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a subida: "))
      (if z_final 
        (IAN:cbe-core ss_entidades_subir "Subir" 0.1 1.0 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para subir. Etapa pulada.")
  )

  ;; --- Etapa de Descida ---
  (princ "\n\n--- Etapa de Descida ---")
  (princ "\nSelecione os blocos e as linhas de apoio para DESCER (ou pressione ENTER para pular):")
  (setq ss_entidades_descer (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_descer 
    (progn 
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a descida: "))
      (if z_final 
        (IAN:cbe-core ss_entidades_descer "Descer" 0.1 1.0 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para descer. Etapa pulada.")
  )

  ;; --- Finalização ---
  (princ "\n\n>>> Construção de Muro de Geogrelha concluída! <<<")
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)

;; --- Lógica para construir um Muro StrataSlope ---
(defun c:CONSTRUIR_MURO_STRATASLOPE (/ *error* old_osmode old_cmdecho ss_base_ss 
                                     caminho_arquivo nome_bloco_ss ent_sel_data 
                                     vla_bloco_modelo first_block_created 
                                     ss_entidades_subir ss_entidades_descer z_final
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
  (setvar "CMDECHO" 0)
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> INICIANDO CONSTRUCAO DE MURO STRATASLOPE <<<")

  ;; --- Etapa 1: Gerar Pontos (GPR) com régua de 1.0 ---
  (princ "\nSelecione a linha de base para o muro:")
  (setq ss_base_ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss_base_ss) 
    (progn (princ "\nLinha de base não selecionada. Comando cancelado.") (exit))
  )

  (setq caminho_arquivo (strcat (getvar "DWGPREFIX") "strataslope_pontos.txt"))
  (princ (strcat "\nArquivo de pontos será salvo em: " caminho_arquivo))

  (if (not (IAN:gerar-pontos-regua-core ss_base_ss 1.0 caminho_arquivo)) 
    (progn (princ "\nFalha ao gerar pontos para o StrataSlope. Comando cancelado.") 
           (exit)
    )
  )
  (princ "\nPontos do eixo gerados com sucesso.")

  ;; --- Etapa 2: Colocar Blocos (CBPM) ---
  (setq nome_bloco_ss nil)
  (while (not nome_bloco_ss) 
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo STRATASLOPE: "))
    (if ent_sel_data 
      (if 
        (= "AcDbBlockReference" 
           (vla-get-ObjectName (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data))))
        )
        (setq nome_bloco_ss (vla-get-EffectiveName vla_bloco_modelo))
        (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
      )
      (progn (princ "\nSeleção de bloco cancelada.") (exit))
    )
  )
  (setq first_block_created (IAN:cbpm-core caminho_arquivo nome_bloco_ss))
  (princ "\n\n--- Primeira fiada de blocos StrataSlope criada. ---")

  ;; --- Etapa 3: Alternar Altura (ABS-Core) com altura de 0.5 ---
  (if (not first_block_created) 
    (progn (princ "\nNenhum bloco foi criado na etapa anterior. Cancelando.") 
           (exit)
    )
  )
  (IAN:abs-core first_block_created 0.5)
  (princ "\n\n--- Altura da primeira fiada alternada automaticamente. ---")

  ;; =================================================================
  ;; --- FASE 2: CONSTRUÇÃO VERTICAL (CBE) ---
  ;; =================================================================

  ;; --- Etapa de Subida ---
  (princ "\n\n--- Etapa de Subida ---")
  (princ "\nSelecione os blocos e linhas de apoio para SUBIR (ou pressione ENTER para pular):")
  (setq ss_entidades_subir (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_subir 
    (progn 
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a subida: "))
      (if z_final 
        (IAN:cbe-core ss_entidades_subir "Subir" 0.05 0.5 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para subir. Etapa pulada.")
  )

  ;; --- Etapa de Descida ---
  (princ "\n\n--- Etapa de Descida ---")
  (princ "\nSelecione os blocos e as linhas de apoio para DESCER (ou pressione ENTER para pular):")
  (setq ss_entidades_descer (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_descer 
    (progn 
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a descida: "))
      (if z_final 
        (IAN:cbe-core ss_entidades_descer "Descer" 0.05 0.5 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para descer. Etapa pulada.")
  )

  ;; --- Finalização ---
  (princ "\n\n>>> Construção de Muro StrataSlope concluída! <<<")
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)

;; --- Lógica para construir um Muro Quadratum ---
(defun c:CONSTRUIR_MURO_QUADRATUM (/ 
                                   *error* old_osmode old_cmdecho
                                   ss_base_quad caminho_arquivo
                                   nome_bloco_quad ent_sel_data vla_bloco_modelo
                                   ss_entidades_subir ss_entidades_descer z_final
                                  )
  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg) (IAN:std-error msg (function (lambda () (if old_cmdecho (setvar "CMDECHO" old_cmdecho)) (IAN:restore-osnap old_osmode)))))

  ;; --- Inicialização ---
  (setq old_cmdecho (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  (setq old_osmode (IAN:disable-osnap))

  (princ "\n>>> INICIANDO CONSTRUCAO DE MURO QUADRATUM <<<")

  ;; --- Etapa 1: Gerar Pontos (GPR) com régua de 2.5 ---
  (princ "\nSelecione a linha de base para o muro:")
  (setq ss_base_quad (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss_base_quad) (progn (princ "\nLinha de base não selecionada. Comando cancelado.") (exit)))

  (setq caminho_arquivo (strcat (getvar "DWGPREFIX") "quadratum_pontos.txt"))
  (princ (strcat "\nArquivo de pontos será salvo em: " caminho_arquivo))
  
  (if (not (IAN:gerar-pontos-regua-core ss_base_quad 2.5 caminho_arquivo))
    (progn (princ "\nFalha ao gerar pontos para o Quadratum. Comando cancelado.") (exit))
  )
  (princ "\nPontos do eixo gerados com sucesso.")

  ;; --- Etapa 2: Colocar Blocos (CBPM) ---
  (setq nome_bloco_quad nil)
  (while (not nome_bloco_quad)
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo QUADRATUM: "))
    (if ent_sel_data
      (if (= "AcDbBlockReference" (vla-get-ObjectName (setq vla_bloco_modelo (vlax-ename->vla-object (car ent_sel_data)))))
        (setq nome_bloco_quad (vla-get-EffectiveName vla_bloco_modelo))
        (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
      )
      (progn (princ "\nSeleção de bloco cancelada.") (exit))
    )
  )
  (IAN:cbpm-core caminho_arquivo nome_bloco_quad)
  (princ "\n\n--- Primeira fiada de blocos Quadratum criada. ---")
  
  ;; =================================================================
  ;; --- FASE 2: CONSTRUÇÃO VERTICAL (CBE) ---
  ;; =================================================================
  
  ;; --- Etapa de Subida ---
  (princ "\n\n--- Etapa de Subida ---")
  (princ "\nSelecione os blocos e linhas de apoio para SUBIR (ou pressione ENTER para pular):")
  (setq ss_entidades_subir (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))
  
  (if ss_entidades_subir
    (progn
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a subida: "))
      (if z_final
        (IAN:cbe-core ss_entidades_subir "Subir" 0.12 0.6 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para subir. Etapa pulada.")
  )

  ;; --- Etapa de Descida ---
  (princ "\n\n--- Etapa de Descida ---")
  (princ "\nSelecione os blocos e as linhas de apoio para DESCER (ou pressione ENTER para pular):")
  (setq ss_entidades_descer (ssget '((0 . "INSERT,LINE,LWPOLYLINE"))))

  (if ss_entidades_descer
    (progn
      (setq z_final (getreal "\nDigite a coordenada Z FINAL para a descida: "))
      (if z_final
        (IAN:cbe-core ss_entidades_descer "Descer" 0.12 0.6 z_final)
        (princ "\nZ Final não informado. Etapa pulada.")
      )
    )
    (princ "\nNenhuma entidade selecionada para descer. Etapa pulada.")
  )

  ;; --- Finalização ---
  (princ "\n\n>>> Construção de Muro Quadratum concluída! <<<")
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
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
