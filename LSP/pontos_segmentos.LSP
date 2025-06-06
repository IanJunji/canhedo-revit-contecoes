;;;; ----------------------------------------------------------------------;;;;
;;;                                                                        ;;;
;;;      IAN LISP Utilities - Ferramentas de Geometria para AutoCAD        ;;;
;;;                                                                        ;;;
;;; ----------------------------------------------------------------------;;;;
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
;;; ----------------------------------------------------------------------;;;;
;;; Autor: ian junji                                                       ;;;
;;; Versão: 0.5                                                            ;;;
;;;; ----------------------------------------------------------------------;;;;

(vl-load-com) ; Necessário para funções VLA (Visual LISP ActiveX)

;;; --- Funções Auxiliares Reutilizáveis ---

(defun IAN:safe-open (path mode / h)
  (cond
    ((setq h (open path mode)) h)
    (t (princ (strcat "\n[ERRO] Falha ao abrir: " path)) nil))
)

(defun IAN:close-handle (h)
  (if h (progn (close h) nil) nil)
)

(defun IAN:std-error (msg restore-fn)
  (if restore-fn
    (progn
      (restore-fn) ; Executa a função de restauração
    )
  )
  (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
    (princ (strcat "\nErro: " msg))
  )
  (princ)
)

(defun IAN:disable-osnap (/ prev)
  (setq prev (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  prev
)

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

(defun c:COMANDOS ( / )
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
(defun IAN:gpr-find-next-point (obj_curva pt_inicio comp_regua tol /
                                mspace melhor_ponto_B circulo_temp_raw circulo_obj
                                pontos_intersecao_variant pontos_intersecao_sa
                                arr_pontos i lista_de_pontos_intersecao
                                param_A_catch param_A menor_param_valido_a_frente 
                                pt_int param_int_catch param_int)
  (princ (strcat "\n  [GPR DEBUG] Régua de " (rtos comp_regua) " a partir de: " (vl-princ-to-string pt_inicio)))
  (setq mspace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq melhor_ponto_B nil)
  (setq circulo_obj nil) 
  (setq centro_circulo_3d pt_inicio)

  (setq circulo_temp_raw (vl-catch-all-apply 'vla-AddCircle 
                                           (list mspace (vlax-3d-point centro_circulo_3d) comp_regua)))

  (if (vl-catch-all-error-p circulo_temp_raw)
    (princ (strcat "\n  [GPR ERRO] Falha ao criar círculo temporário: " (vl-catch-all-error-message circulo_temp_raw)))
    (progn
      (setq circulo_obj circulo_temp_raw) 

      (setq pontos_intersecao_variant (vla-IntersectWith obj_curva circulo_obj acExtendNone))
      (vla-Delete circulo_obj) 
      (setq pontos_intersecao_sa (vlax-variant-value pontos_intersecao_variant))

      (if (and pontos_intersecao_sa (eq (type pontos_intersecao_sa) 'SAFEARRAY)
               (= (vlax-safearray-get-dim pontos_intersecao_sa) 1)
               (> (vlax-safearray-get-u-bound pontos_intersecao_sa 1) -1))
        (progn
          (setq lista_de_pontos_intersecao nil)
          (setq arr_pontos (vlax-safearray->list pontos_intersecao_sa))
          (setq i 0)
          (while (< i (length arr_pontos))
            (setq lista_de_pontos_intersecao (cons (list (nth i arr_pontos) (nth (1+ i) arr_pontos) (nth (+ i 2) arr_pontos)) lista_de_pontos_intersecao))
            (setq i (+ i 3))
          )
          (setq lista_de_pontos_intersecao (reverse lista_de_pontos_intersecao))
          (princ (strcat "\n    [GPR DEBUG] " (itoa (length lista_de_pontos_intersecao)) " interseções cruas: " (vl-princ-to-string lista_de_pontos_intersecao)))

          (setq param_A_catch (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list obj_curva pt_inicio)))
          (if (vl-catch-all-error-p param_A_catch)
            (progn (princ (strcat "\n    [GPR ERRO] Parâmetro pt_inicio: " (vl-catch-all-error-message param_A_catch))) (setq param_A nil))
            (setq param_A param_A_catch)
          )
          (if param_A
            (progn
              (setq menor_param_valido_a_frente nil)
              (foreach pt_int lista_de_pontos_intersecao
                (if (and (> (distance pt_int pt_inicio) tol) (equal (distance pt_int pt_inicio) comp_regua tol))
                  (progn
                    (setq param_int_catch (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list obj_curva pt_int)))
                    (if (not (vl-catch-all-error-p param_int_catch))
                      (progn
                        (setq param_int param_int_catch)
                        (if (> param_int (+ param_A (* tol 0.01)))
                          (if (or (null menor_param_valido_a_frente) (< param_int menor_param_valido_a_frente))
                            (progn (setq menor_param_valido_a_frente param_int) (setq melhor_ponto_B pt_int))
                          )
                        )
                      )
                      (princ (strcat "\n    [GPR ERRO] Parâmetro pt_int " (vl-princ-to-string pt_int) ": " (vl-catch-all-error-message param_int_catch)))
                    )
                  )
                )
              )
              (if melhor_ponto_B
                (princ (strcat "\n    [GPR DEBUG] Ponto B selecionado: " (vl-princ-to-string melhor_ponto_B)))
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
                               ponto_A ponto_B lista_pontos_entidade_atual primeiro_ponto_adicionado
                               tolerancia start_pt_entidade_catch old_osmode)
  (defun *error* (msg)
    (IAN:std-error msg
                   (function
                     (lambda ()
                       (setq f (IAN:close-handle f))
                       (IAN:restore-osnap old_osmode))))
  )
  (setq old_osmode (IAN:disable-osnap))
  (setq tolerancia 0.0001) (setq comprimento_regua nil)
  (princ "\n>>> GERADOR DE PONTOS COM RÉGUA (GPR) <<<")
  (princ "\nSelecione as entidades alvo (linhas, polilinhas, arcos, splines): ")
  (setq ss_linhas_alvo (ssget '((0 . "LINE,LWPOLYLINE,ARC,SPLINE"))))

  (if (null ss_linhas_alvo) (progn (princ "\nNenhuma entidade selecionada. Comando cancelado.") (IAN:restore-osnap old_osmode) (exit)))

  (while (or (null comprimento_regua) (<= comprimento_regua 0.0))
    (setq comprimento_regua (getdist "\nDigite o comprimento da régua/segmento: "))
    (if (or (null comprimento_regua) (<= comprimento_regua 0.0)) (princ "\nComprimento inválido. Deve ser positivo."))
  )
  (setq arquivo_saida (getfiled "Salvar arquivo de coordenadas TXT" "" "txt" 1))
  (if (null arquivo_saida) (progn (princ "\nNenhum arquivo selecionado. Comando cancelado.") (IAN:restore-osnap old_osmode) (exit)))

  (setq lista_todos_os_pontos nil) (setq contador 0)
  (princ (strcat "\nProcessando " (itoa (sslength ss_linhas_alvo)) " entidade(s)..."))

  (while (< contador (sslength ss_linhas_alvo))
    (setq ent_nome (ssname ss_linhas_alvo contador))
    (setq ent_obj (vlax-ename->vla-object ent_nome))
    (princ (strcat "\nAnalisando entidade " (itoa (1+ contador)) " (" (vla-get-ObjectName ent_obj) "):"))
    (setq lista_pontos_entidade_atual nil) (setq primeiro_ponto_adicionado nil)
    
    (setq start_pt_entidade_catch (vl-catch-all-apply 'vlax-curve-getStartPoint (list ent_obj)))
    (if (vl-catch-all-error-p start_pt_entidade_catch)
      (progn
        (princ (strcat "\n  [GPR AVISO] Falha ao obter ponto inicial. Pulando. (" (vl-catch-all-error-message start_pt_entidade_catch) ")"))
        (setq ponto_A nil)
      )
      (setq ponto_A start_pt_entidade_catch)
    )
    (while ponto_A 
      (setq ponto_B (IAN:gpr-find-next-point ent_obj ponto_A comprimento_regua tolerancia))
      (if ponto_B
        (progn
          (if (not primeiro_ponto_adicionado)
            (progn (setq lista_pontos_entidade_atual (cons ponto_A lista_pontos_entidade_atual)) (setq primeiro_ponto_adicionado T))
          )
          (setq lista_pontos_entidade_atual (cons ponto_B lista_pontos_entidade_atual))
          (setq ponto_A ponto_B)
        )
        (setq ponto_A nil)
      )
    )
    (if lista_pontos_entidade_atual
      (progn
        (setq lista_pontos_entidade_atual (reverse lista_pontos_entidade_atual))
        (setq lista_todos_os_pontos (append lista_todos_os_pontos lista_pontos_entidade_atual))
        (princ (strcat " -> " (itoa (length lista_pontos_entidade_atual)) " pontos."))
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
          (princ (strcat "\n\nEscrevendo " (itoa (length lista_todos_os_pontos)) " pontos em: " arquivo_saida))
          (foreach pt lista_todos_os_pontos
            (write-line (strcat (rtos (car pt) 2 8) "," (rtos (cadr pt) 2 8) "," (rtos (caddr pt) 2 8)) f)
          )
          (setq f (IAN:close-handle f))
          (princ "\nSucesso! Arquivo salvo.")
        )
        (princ (strcat "\n[GPR ERRO] Falha ao abrir arquivo para escrita: " arquivo_saida))
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
(defun IAN:cbpm-string-to-pointlist (str_coords / str-split-by-comma str-to-real coord_parts x_str y_str z_str pt_x pt_y pt_z)
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
              (princ (strcat "\n  [CBPM DEBUG] Falha conversão X,Y,Z: " str_coords 
                             " (X:" (if pt_x "OK" (nth 0 coord_parts)) 
                             " Y:" (if pt_y "OK" (nth 1 coord_parts)) 
                             " Z:" (if pt_z "OK" (nth 2 coord_parts)) ")"))
              nil
            )
          )
        )
        (progn (princ (strcat "\n  [CBPM DEBUG] Linha não contém 3 partes: " str_coords)) nil)
      )
    )
    nil 
  )
)

;;; --- Função Auxiliar para CBPM: Garantir Existência da Layer ---
(defun IAN:cbpm-ensure-layer (layer_name color_index / acadDoc layersColl layerObj_raw vla_layer current_cmdecho) ; Renomeado layerObj para layerObj_raw
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
        (princ (strcat "\n  [CBPM ERRO] Falha ao criar layer '" layer_name "': " (vl-catch-all-error-message layerObj_raw)))
        (progn 
          (setq vla_layer layerObj_raw) 
          (if (and color_index (vlax-method-applicable-p vla_layer 'put_Color))
             (vl-catch-all-apply 'vla-put-Color (list vla_layer color_index))
          )
          (if (vlax-method-applicable-p vla_layer 'put_Lock) (vl-catch-all-apply 'vla-put-Lock (list vla_layer :vlax-false)))
          (if (vlax-method-applicable-p vla_layer 'put_Freeze) (vl-catch-all-apply 'vla-put-Freeze (list vla_layer :vlax-false)))
          (if (vlax-method-applicable-p vla_layer 'put_Plottable) (vl-catch-all-apply 'vla-put-Plottable (list vla_layer :vlax-true)))
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
(defun c:COLOCAR_BLOCO_EM_PONTOS_MEDIOS ( / *error* arquivo_txt f linha lineno
                                        lista_pontos_lidos p_temp i
                                        p_orig_A p_orig_B p_medio_calc
                                        lista_segmentos_para_blocos
                                        ent_sel_data nome_bloco_real vla_bloco_selecionado_raw vla_bloco_modelo
                                        layer_alvo_nome vla_layer_alvo angulo_insercao old_clayer
                                        contador_blocos_inseridos mspace doc tolerancia_dist
                                        selecao_valida vla_bloco_inserido_raw old_osmode)
  (defun *error* (msg)
    (IAN:std-error msg
                   (function
                     (lambda ()
                       (setq f (IAN:close-handle f))
                       (if old_clayer (setvar "CLAYER" old_clayer))
                       (IAN:restore-osnap old_osmode))))
  )
  (setq old_osmode (IAN:disable-osnap))
  (setq layer_alvo_nome "BLOCOS_PONTOS_MEDIOS")
  (setq contador_blocos_inseridos 0)
  (setq lista_pontos_lidos nil)
  (setq lista_segmentos_para_blocos nil)
  (setq tolerancia_dist 0.00001) 
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace (vla-get-ModelSpace doc))
  (setq old_clayer (getvar "CLAYER")) 

  (princ "\n>>> INSERIR BLOCOS EM PONTOS MÉDIOS (CBPM) <<<")

  (setq arquivo_txt (getfiled "Selecione o arquivo TXT com coordenadas (X,Y,Z)" "" "txt" 0))
  (if (or (null arquivo_txt) (not (findfile arquivo_txt)))
    (progn (princ "\nArquivo não selecionado/encontrado. Comando cancelado.") (IAN:restore-osnap old_osmode) (exit))
  )
  (setq f (IAN:safe-open arquivo_txt "r"))
  (if (not f) (progn (princ (strcat "\n[CBPM ERRO] Falha ao abrir arquivo: " arquivo_txt)) (IAN:restore-osnap old_osmode) (exit)))
  
  (princ (strcat "\nLendo arquivo: " arquivo_txt))
  (setq lineno 0)
  (while (setq linha (read-line f))
    (setq lineno (1+ lineno))
    (setq p_temp (IAN:cbpm-string-to-pointlist linha))
    (if p_temp
      (setq lista_pontos_lidos (cons p_temp lista_pontos_lidos))
      (princ (strcat "\n  [CBPM AVISO] Linha " (itoa lineno) " ignorada: \"" linha "\""))
    )
  )
  (setq f (IAN:close-handle f))
  (setq lista_pontos_lidos (reverse lista_pontos_lidos)) 

  (if (< (length lista_pontos_lidos) 2)
    (progn (princ "\n[CBPM ERRO] Menos de 2 pontos válidos lidos. Comando cancelado.") (IAN:restore-osnap old_osmode) (exit))
  )
  (princ (strcat "\n" (itoa (length lista_pontos_lidos)) " pontos válidos lidos."))

  (setq i 0)
  (while (< i (1- (length lista_pontos_lidos))) 
    (setq p_orig_A (nth i lista_pontos_lidos))
    (setq p_orig_B (nth (1+ i) lista_pontos_lidos))
    (if (> (distance p_orig_A p_orig_B) tolerancia_dist)
        (progn
            (setq p_medio_calc (list (/ (+ (car p_orig_A) (car p_orig_B)) 2.0)
                                     (/ (+ (cadr p_orig_A) (cadr p_orig_B)) 2.0)
                                     (/ (+ (caddr p_orig_A) (caddr p_orig_B)) 2.0)))
            (setq lista_segmentos_para_blocos (cons (list p_medio_calc p_orig_A p_orig_B) lista_segmentos_para_blocos))
        )
        (princ (strcat "\n  [CBPM AVISO] Segmento " (itoa (1+ i)) "-" (itoa (+ i 2)) " ignorado (curto/coincidente)."))
    )
    (setq i (1+ i))
  )
  (setq lista_segmentos_para_blocos (reverse lista_segmentos_para_blocos))

  (if (null lista_segmentos_para_blocos)
    (progn (princ "\n[CBPM ERRO] Nenhum segmento válido para pontos médios. Verifique o arquivo.") (IAN:restore-osnap old_osmode) (exit))
  )
  (princ (strcat "\n" (itoa (length lista_segmentos_para_blocos)) " pontos médios calculados."))

  (setq selecao_valida nil) (setq nome_bloco_real nil) (setq vla_bloco_modelo nil)
  (while (not selecao_valida)
    (setq ent_sel_data (entsel "\nSelecione o bloco modelo a ser inserido: "))
    (if ent_sel_data
      (progn
        (setq vla_bloco_selecionado_raw (vl-catch-all-apply 'vlax-ename->vla-object (list (car ent_sel_data))))
        (if (not (vl-catch-all-error-p vla_bloco_selecionado_raw))
          (progn
            (setq vla_bloco_modelo vla_bloco_selecionado_raw) 
            (if (and vla_bloco_modelo (vlax-property-available-p vla_bloco_modelo 'ObjectName) 
                     (= (vla-get-ObjectName vla_bloco_modelo) "AcDbBlockReference"))
              (progn
                (setq nome_bloco_real (vla-get-EffectiveName vla_bloco_modelo))
                (princ (strcat "\nBloco selecionado: '" nome_bloco_real "'."))
                (setq selecao_valida T)
              )
              (princ "\nSeleção inválida. A entidade não é um bloco. Tente novamente.")
            )
          )
          (princ (strcat "\n[CBPM ERRO] Conversão para objeto VLA: " (vl-catch-all-error-message vla_bloco_selecionado_raw)))
        )
      )
      (progn (princ "\nSeleção cancelada. Comando encerrado.") (IAN:restore-osnap old_osmode) (exit))
    )
  )
  (setq vla_layer_alvo (IAN:cbpm-ensure-layer layer_alvo_nome 7)) 
  (if (not vla_layer_alvo) 
    (progn
      (princ (strcat "\n[CBPM AVISO] Falha layer '" layer_alvo_nome "'. Usando layer atual: " old_clayer))
      (setq layer_alvo_nome old_clayer) 
    )
  )
  (princ (strcat "\nInserindo blocos na layer '" layer_alvo_nome "'..."))
  (foreach segmento lista_segmentos_para_blocos
    (setq p_medio_calc (car segmento))
    (setq p_orig_A     (cadr segmento))
    (setq p_orig_B     (caddr segmento))
    (setq angulo_insercao (angle p_orig_A p_orig_B)) 

    (setq vla_bloco_inserido_raw (vl-catch-all-apply 'vla-InsertBlock
                                  (list mspace (vlax-3d-point p_medio_calc) nome_bloco_real 
                                        1.0 1.0 1.0 angulo_insercao)))
    (if (not (vl-catch-all-error-p vla_bloco_inserido_raw))
      (progn
        (setq contador_blocos_inseridos (1+ contador_blocos_inseridos))
        (if (and vla_layer_alvo (vlax-method-applicable-p vla_bloco_inserido_raw 'put_Layer))
            (vla-put-Layer vla_bloco_inserido_raw layer_alvo_nome)
            (princ (strcat "\n  [CBPM AVISO] Não foi possível definir a layer para o bloco inserido no ponto " (vl-princ-to-string p_medio_calc)))
        )
        (princ ".") 
      )
      (princ (strcat "\n  [CBPM ERRO] Falha ao inserir bloco '" nome_bloco_real "' em " (vl-princ-to-string p_medio_calc) 
                     ": " (vl-catch-all-error-message vla_bloco_inserido_raw)))
    )
  )
  (if (> contador_blocos_inseridos 0)
    (princ (strcat "\nSucesso! " (itoa contador_blocos_inseridos) " bloco(s) '" nome_bloco_real "' inserido(s)."))
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

(defun c:IAN_RotateThenOffsetBlock ( /
                                    ;; Variáveis locais
                                    *error*
                                    s:old_cmdecho s:old_osmode ; Variáveis de sistema
                                    offset_distance
                                    ss_blocks total_selected i
                                    ent_name vla_block
                                    current_insertion_point_vla current_insertion_point_list
                                    current_rotation_rad
                                    new_block_rotation_rad
                                    offset_angle_rad
                                    new_ins_x new_ins_y new_ins_z
                                    new_insertion_point_list
                                    processed_count
                                    error_obj
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
  (setq s:old_cmdecho (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  (setq s:old_osmode (getvar "OSMODE"))   (setvar "OSMODE" 0)
  (setq processed_count 0)

  ;; 1. Pedir Comprimento do Offset
  (initget 1) ; Não pode ser nulo
  (setq offset_distance (getdist "\nDigite a distância para o deslocamento perpendicular (ex: 8.0): "))
  (if (null offset_distance)
    (progn (princ "\nDistância não fornecida. Comando cancelado.") (*error* "Cancelado pelo usuário") (exit))
  )

  ;; 2. Selecionar Blocos
  (princ "\nSelecione os blocos para processar:")
  (setq ss_blocks (ssget '((0 . "INSERT"))))

  (if (null ss_blocks)
    (progn (princ "\nNenhum bloco selecionado.") (*error* "Seleção cancelada") (exit))
  )

  (setq total_selected (sslength ss_blocks))
  (princ (strcat "\n" (itoa total_selected) " bloco(s) selecionado(s). Processando..."))

  ;; 3. Para Cada Bloco Selecionado
  (setq i 0)
  (while (< i total_selected)
    (setq ent_name (ssname ss_blocks i))
    (setq vla_block (vl-catch-all-apply 'vlax-ename->vla-object (list ent_name)))

    (if (vl-catch-all-error-p vla_block)
      (princ (strcat "\n  AVISO: Falha ao obter objeto VLA para " (vl-princ-to-string ent_name) ". Erro: " (vl-catch-all-error-message vla_block)))
      (progn
        (setq vla_block vla_block) ; Dereference
        (princ (strcat "\n  Processando Bloco (Handle: " (if (vlax-property-available-p vla_block 'Handle) (vla-get-Handle vla_block) "N/A") ")..."))

        ;; Obter Ponto de Inserção Atual
        (setq current_insertion_point_vla (vl-catch-all-apply 'vla-get-InsertionPoint (list vla_block)))
        (setq current_rotation_rad (vl-catch-all-apply 'vla-get-Rotation (list vla_block)))

        (if (or (vl-catch-all-error-p current_insertion_point_vla)
                (vl-catch-all-error-p current_rotation_rad))
          (princ (strcat "\n    AVISO: Falha ao obter Ponto de Inserção ou Rotação."))
          (progn
            (setq current_insertion_point_list (vlax-safearray->list (vlax-variant-value current_insertion_point_vla)))
            (setq current_rotation_rad current_rotation_rad) ; Já é o valor

            ;; Rotacionar o Bloco em 180 graus em torno do ponto de inserção
            (setq new_block_rotation_rad (+ current_rotation_rad PI))
            (setq error_obj (vl-catch-all-apply 'vla-put-Rotation (list vla_block new_block_rotation_rad)))

            (if (vl-catch-all-error-p error_obj)
              (princ (strcat "\n    ERRO: Falha ao aplicar rotação de 180 graus: " (vl-catch-all-error-message error_obj)))
              (progn
                (princ (strcat "\n    Bloco rotacionado para " (angtos new_block_rotation_rad 0 4) " graus."))
                
                ;; Calcular o ângulo para o deslocamento (90 graus da nova direção do bloco)
                (setq offset_angle_rad (- new_block_rotation_rad (/ PI 2.0))) ; PI/2 = 90 graus

                ;; Calcular o novo ponto de inserção
                (setq new_ins_x (+ (car current_insertion_point_list) (* offset_distance (cos offset_angle_rad))))
                (setq new_ins_y (+ (cadr current_insertion_point_list) (* offset_distance (sin offset_angle_rad))))
                (setq new_ins_z (caddr current_insertion_point_list)) ; Mantém o Z original
                (setq new_insertion_point_list (list new_ins_x new_ins_y new_ins_z))

                ;; Mover o Bloco (atualizar seu ponto de inserção)
                (setq error_obj (vl-catch-all-apply 'vla-put-InsertionPoint (list vla_block (vlax-3d-point new_insertion_point_list))))

                (if (vl-catch-all-error-p error_obj)
                  (princ (strcat "\n    ERRO: Falha ao mover o bloco para o novo ponto: " (vl-catch-all-error-message error_obj)))
                  (progn
                    (princ (strcat "\n    Bloco movido para: " (vl-princ-to-string new_insertion_point_list)))
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
    (princ (strcat "\nSucesso! " (itoa processed_count) " de " (itoa total_selected) " bloco(s) foram processados."))
    (princ (strcat "\nNenhum bloco foi processado com sucesso de " (itoa total_selected) " selecionado(s). Verifique os avisos.")))
  (princ "\n-----------------------------------------------------")

  ;; --- Limpeza Final ---
  (if s:old_cmdecho (setvar "CMDECHO" s:old_cmdecho))
  (if s:old_osmode (setvar "OSMODE" s:old_osmode))
  (princ "\nComando RTOB v1.0 concluído.")
  (princ)
)

;; Alias para o comando
(defun c:RTOB () (c:IAN_RotateThenOffsetBlock))
(princ "\nComando RTOB v1.0 (Rotacionar e Deslocar) carregado. Digite RTOB para executar.")
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

;;;
;;; c:CRIAR_CAMADAS_BLOCO_ESCADINHA (CBE)
;;;
(defun c:CRIAR_CAMADAS_BLOCO_ESCADINHA ( /
                                       *error* old_cmdecho old_clayer old_osmode
                                       ss_blocos_base selection_count i ent_name vla_bloco_base
                                       bloco_base_process_list ; Lista de (list VLA_Bloco_Base PontoInsercaoBase RotacaoBase NomeBlocoBase)
                                       direcao_escada_str direcao_sinal_z
                                       offset_h alt_v z_final
                                       qnt_novas_camadas
                                       angulo_direcao_offset_h 
                                       offset_vetor_unit_x offset_vetor_unit_y
                                       n offset_h_acum novo_x novo_y novo_z ponto_insercao_nova_camada
                                       vla_bloco_copiado layer_bloco_original
                                       total_blocos_criados blocos_criados_nesta_base
                                       doc mspace
                                     )

  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg)
    (IAN:std-error msg
                   (function
                     (lambda ()
                       (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
                       (if old_clayer (setvar "CLAYER" old_clayer))
                       (IAN:restore-osnap old_osmode))))
  )

  ;; --- Inicialização ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq old_clayer (getvar "CLAYER"))
  (setq old_osmode (IAN:disable-osnap))
  (setq total_blocos_criados 0)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace (vla-get-ModelSpace doc))

  (princ "\n>>> CRIAR CAMADAS DE BLOCO EM ESCADINHA (CBE) <<<")

  ;; --- Etapa 1: Selecionar Blocos da Camada Base ---
  (princ "\nSelecione os blocos da camada base:")
  (setq ss_blocos_base (ssget '((0 . "INSERT")))) ; Filtra para selecionar apenas blocos
  (if (null ss_blocos_base)
    (progn (princ "\nNenhum bloco selecionado. Comando cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit))
  )
  (setq selection_count (sslength ss_blocos_base))
  (princ (strcat "\n" (itoa selection_count) " bloco(s) base selecionado(s)."))

  ;; --- Etapa 2: Inputs do Usuário ---
  (initget "Subir Descer")
  (setq direcao_escada_str (getkword "\nA escadinha será para [Subir/Descer] em relação à camada base?: "))
  (if (null direcao_escada_str) (progn (princ "\nOpção inválida. Comando Cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit)))
  (setq direcao_sinal_z (if (eq direcao_escada_str "Subir") 1.0 -1.0))

  (setq offset_h (getdist "\nDigite o offset horizontal por camada (recuo/avanço): "))
  (if (null offset_h) (progn (princ "\nOffset horizontal não fornecido. Comando Cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit)))

  (setq alt_v (getdist "\nDigite a altura vertical por camada (altura do bloco/degrau): "))
  (if (or (null alt_v) (<= alt_v 0.0))
    (progn (princ "\nAltura vertical inválida ou zero. Comando Cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit)))

  (setq z_final (getreal "\nDigite a coordenada Z final desejada para a última camada: "))
  (if (null z_final) (progn (princ "\nZ final não fornecido. Comando Cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit)))

  ;; --- Etapa 3: Coletar Informações dos Blocos Base ---
  (setq bloco_base_process_list nil)
  (setq i 0)
  (while (< i selection_count)
    (setq ent_name (ssname ss_blocos_base i))
    (setq vla_bloco_base (vl-catch-all-apply 'vlax-ename->vla-object (list ent_name)))
    (if (not (vl-catch-all-error-p vla_bloco_base))
      (setq vla_bloco_base vla_bloco_base) ; Já é o objeto VLA
      (progn (princ (strcat "\n  [CBE AVISO] Falha ao obter VLA object para entidade selecionada: " (vl-princ-to-string ent_name))) (setq vla_bloco_base nil))
    )
    
    (if vla_bloco_base
      (if (and (vlax-property-available-p vla_bloco_base 'ObjectName) (= (vla-get-ObjectName vla_bloco_base) "AcDbBlockReference"))
        (progn
          (setq pt_base_variant (vlax-get-property vla_bloco_base 'InsertionPoint))
          (setq pt_base_safearray (vlax-variant-value pt_base_variant))
          (setq pt_base_list (vlax-safearray->list pt_base_safearray))
          (setq rot_base (vlax-get-property vla_bloco_base 'Rotation))
          (setq nome_base (vla-get-EffectiveName vla_bloco_base))
          (setq layer_bloco_original (vla-get-Layer vla_bloco_base))
          (setq bloco_base_process_list 
                (cons (list vla_bloco_base pt_base_list rot_base nome_base layer_bloco_original) bloco_base_process_list)
          )
        )
        (princ (strcat "\n  [CBE AVISO] Entidade " (vl-princ-to-string ent_name) " não é um bloco e será ignorada."))
      )
    )
    (setq i (1+ i))
  )
  (setq bloco_base_process_list (reverse bloco_base_process_list))

  (if (null bloco_base_process_list)
    (progn (princ "\nNenhum bloco válido encontrado na seleção. Comando cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit)))

  ;; --- Etapa 4: Processar cada Bloco Base e Criar Camadas ---
  (princ "\nCriando camadas em escadinha...")
  (foreach bloco_info bloco_base_process_list
    (setq vla_bloco_base      (nth 0 bloco_info))
    (setq pt_base_list        (nth 1 bloco_info))
    (setq rot_base            (nth 2 bloco_info))
    (setq nome_base           (nth 3 bloco_info))
    (setq layer_bloco_original (nth 4 bloco_info))
    
    (setq z_atual_bloco (caddr pt_base_list))
    (setq blocos_criados_nesta_base 0)

    ;; Calcular número de novas camadas
    (setq delta_z_total (- z_final z_atual_bloco))
    (if (eq direcao_escada_str "Subir")
        (if (<= delta_z_total 0) (setq qnt_novas_camadas 0) (setq qnt_novas_camadas (fix (/ delta_z_total alt_v))))
        (if (>= delta_z_total 0) (setq qnt_novas_camadas 0) (setq qnt_novas_camadas (fix (/ (abs delta_z_total) alt_v))))
    )
    
    (if (> qnt_novas_camadas 0)
      (progn
        (princ (strcat "\n  Processando bloco base (Handle: " (vla-get-Handle vla_bloco_base) "), criando " (itoa qnt_novas_camadas) " camada(s)..."))
        
        ;; Determinar direção do offset horizontal (perpendicular à rotação do bloco base)
        ;; Se "descendo", recua para a "direita" (-90 graus). Se "subindo", recua para a "esquerda" (+90 graus).
        ;; Esta convenção pode precisar de ajuste visual.
        (setq angulo_direcao_offset_h 
              (if (eq direcao_escada_str "Subir")
                  (+ rot_base (/ PI 2.0))  ; +90 graus (esquerda do bloco, se frente é X local)
                  (- rot_base (/ PI 2.0))  ; -90 graus (direita do bloco)
              )
        )
        (setq offset_vetor_unit_x (cos angulo_direcao_offset_h))
        (setq offset_vetor_unit_y (sin angulo_direcao_offset_h))

        (setq n 1) ; Contador para as novas camadas
        (while (<= n qnt_novas_camadas)
          (setq offset_h_acum (* n offset_h))
          
          (setq novo_x (+ (car pt_base_list) (* offset_h_acum offset_vetor_unit_x)))
          (setq novo_y (+ (cadr pt_base_list) (* offset_h_acum offset_vetor_unit_y)))
          (setq novo_z (+ z_atual_bloco (* n direcao_sinal_z alt_v)))
          
          (setq ponto_insercao_nova_camada (list novo_x novo_y novo_z))

          ;; Inserir uma nova instância do bloco original
          (setq vla_bloco_copiado 
                (vl-catch-all-apply 'vla-InsertBlock 
                                    (list mspace 
                                          (vlax-3d-point ponto_insercao_nova_camada)
                                          nome_base  ; Nome do bloco original da base
                                          1.0 1.0 1.0 ; Escalas
                                          rot_base   ; Mesma rotação do bloco base
                                    )
                )
          )
          
          (if (not (vl-catch-all-error-p vla_bloco_copiado))
            (progn
              (vla-put-Layer vla_bloco_copiado layer_bloco_original) ; Coloca na mesma layer do original
              (setq total_blocos_criados (1+ total_blocos_criados))
              (setq blocos_criados_nesta_base (1+ blocos_criados_nesta_base))
              (princ ".")
            )
            (princ (strcat "\n    [CBE ERRO] Falha ao inserir bloco '" nome_base "' na camada " (itoa n) 
                           " para o bloco base (Handle: " (vla-get-Handle vla_bloco_base) "). Erro: " 
                           (vl-catch-all-error-message vla_bloco_copiado)))
          )
          (setq n (1+ n))
        ) ; Fim While N
        (if (> blocos_criados_nesta_base 0) (princ (strcat " " (itoa blocos_criados_nesta_base) " blocos adicionados.")))
      )
      ;; (princ (strcat "\n  Bloco base (Handle: " (vla-get-Handle vla_bloco_base) ") já está no Z final ou além. Nenhuma camada criada.")) ; Debug opcional
    )
  ) ; Fim Foreach

  ;; --- Feedback Final ---
  (if (> total_blocos_criados 0)
    (princ (strcat "\nSucesso! " (itoa total_blocos_criados) " novo(s) bloco(s) criado(s) em escadinha."))
    (princ "\nNenhum novo bloco foi criado (verifique o Z final e os blocos base).")
  )

  ;; --- Finalização ---
  (setvar "CMDECHO" old_cmdecho)
  (setvar "CLAYER" old_clayer) ; Restaura layer original que estava ativa
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

;;;
;;; IAN:abs-get-block-instances
;;; Obtém uma lista de objetos VLA de todas as instâncias de um bloco específico.
;;;
;;; Argumentos:
;;;   target_block_name (String) : O nome efetivo do bloco a ser procurado.
;;;
;;; Retorna:
;;;   (List) : Lista de VLA-Objects das instâncias do bloco, ou nil.
;;;
(defun IAN:abs-get-block-instances (target_block_name / ss selection_count i ent_name vla_obj instance_list)
  (setq instance_list nil)
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 2 target_block_name))))
    (progn
      (setq selection_count (sslength ss))
      (setq i 0)
      (while (< i selection_count)
        (setq ent_name (ssname ss i))
        (setq vla_obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent_name)))
        (if (not (vl-catch-all-error-p vla_obj))
          (setq instance_list (cons vla_obj instance_list)))
        (setq i (1+ i))))
    ;; (princ (strcat "\nNenhuma instância do bloco '" target_block_name "' encontrada com ssget.")) ; Debug opcional
  )
  (if instance_list (reverse instance_list) nil)
)

;;;
;;; IAN:abs-get-insertion-point-list
;;; Obtém o ponto de inserção de um objeto VLA de bloco como uma lista LISP (X Y Z).
;;;
;;; Argumentos:
;;;   block_vla (VLA-Object) : O objeto VLA do bloco.
;;;
;;; Retorna:
;;;   (List) : Lista (X Y Z) do ponto de inserção, ou nil se falhar.
;;;
(defun IAN:abs-get-insertion-point-list (block_vla / pt_variant pt_safearray)
  (if (and block_vla (vlax-property-available-p block_vla 'InsertionPoint))
    (progn
      (setq pt_variant (vl-catch-all-apply 'vlax-get-property (list block_vla 'InsertionPoint)))
      (if (not (vl-catch-all-error-p pt_variant))
        (progn
          (setq pt_safearray (vl-catch-all-apply 'vlax-variant-value (list pt_variant)))
          (if (not (vl-catch-all-error-p pt_safearray))
            (if (and pt_safearray (eq (type pt_safearray) 'SAFEARRAY))
              (vlax-safearray->list pt_safearray) ; Retorna a lista LISP
              nil ; Não era um safearray
            )
            nil ; Erro ao converter variant para safearray
          )
        )
        nil ; Erro ao obter a propriedade (variant)
      )
    )
    nil ; Objeto de bloco inválido ou propriedade não disponível
  )
)

;;;
;;; IAN:abs-sort-blocks-by-proximity
;;; Ordena uma lista de VLA-Objects de blocos pela proximidade,
;;; começando com um bloco inicial especificado.
;;;
;;; Argumentos:
;;;   first_block_vla      (VLA-Object) : O bloco inicial da sequência.
;;;   candidate_blocks_vla (List)       : Lista de VLA-Objects candidatos (deve incluir first_block_vla).
;;;
;;; Retorna:
;;;   (List) : Lista ordenada de VLA-Objects.
;;;
(defun IAN:abs-sort-blocks-by-proximity (first_block_vla candidate_blocks_vla /
                                         ordered_list current_block_obj remaining_candidates_copy
                                         current_block_pt closest_next_block shortest_dist_found
                                         candidate_obj_loop candidate_pt_loop current_iter_dist)
  (setq ordered_list nil)
  (setq current_block_obj first_block_vla)
  (setq remaining_candidates_copy (append candidate_blocks_vla nil)) ; Copia para poder modificar

  (if (not current_block_obj)
    (progn
      (princ "\n[ABS ERRO INTERNO] Bloco inicial inválido para ordenação.")
      (setq ordered_list nil) ; Garante que retorna nil se o início for ruim
    )
    ;; Loop principal: continua enquanto tivermos um bloco atual para processar
    (while current_block_obj
      (setq ordered_list (cons current_block_obj ordered_list)) ; Adiciona o bloco atual à lista ordenada

      ;; Remove o bloco atual da lista de candidatos restantes
      (setq remaining_candidates_copy
            (vl-remove-if
              '(lambda (blk)
                 (if (and blk current_block_obj
                          (vlax-property-available-p blk 'Handle)
                          (vlax-property-available-p current_block_obj 'Handle))
                   (equal (vla-get-Handle blk) (vla-get-Handle current_block_obj))
                   nil
                 )
               )
              remaining_candidates_copy
            )
      )
      
      ;; Se ainda houver candidatos, procura o próximo mais próximo
      (if remaining_candidates_copy
        (progn
          (setq current_block_pt (IAN:abs-get-insertion-point-list current_block_obj))
          (setq closest_next_block nil)
          (setq shortest_dist_found nil)

          (if current_block_pt ; Só procura se o ponto do bloco atual foi obtido
            (foreach candidate_obj_loop remaining_candidates_copy
              (setq candidate_pt_loop (IAN:abs-get-insertion-point-list candidate_obj_loop))
              (if candidate_pt_loop ; Só considera o candidato se seu ponto foi obtido
                (progn
                  (setq current_iter_dist (distance current_block_pt candidate_pt_loop))
                  (if (or (null shortest_dist_found) (< current_iter_dist shortest_dist_found))
                    (progn
                      (setq shortest_dist_found current_iter_dist)
                      (setq closest_next_block candidate_obj_loop)
                    )
                  )
                )
              )
            )
            ;; Se não conseguiu o ponto do current_block_obj, não pode encontrar o próximo
            (princ (strcat "\n[ABS AVISO] Não foi possível obter o ponto do bloco atual (Handle: " 
                           (if (and current_block_obj (vlax-property-available-p current_block_obj 'Handle)) (vla-get-Handle current_block_obj) "N/A") 
                           ") para continuar a ordenação."))
          )
          (setq current_block_obj closest_next_block) ; Define o próximo bloco a ser processado
        )
        (setq current_block_obj nil) ; Não há mais candidatos, encerra o loop while
      )
    )
  )
  (if ordered_list (reverse ordered_list) nil)
)

;;;
;;; c:ALTBLOCOSEQ (Alterar Altura de Blocos em Sequência)
;;; Comando principal.
;;;
(defun c:ALTBLOCOSEQ ( /
                       *error*                           ; Manipulador de erro local
                       old_cmdecho old_osmode            ; Para restaurar CMDECHO e OSMODE
                       first_block_ename first_block_vla first_block_name ; Bloco inicial
                       height_offset                     ; Altura de deslocamento Z
                       all_instances_of_block            ; Lista de todos os blocos com o mesmo nome
                       ordered_block_sequence            ; Blocos ordenados por proximidade
                       block_vla_in_loop index           ; Para iteração
                       current_insertion_pt_list         ; Ponto de inserção atual como lista LISP
                       new_insertion_pt_list             ; Novo ponto de inserção como lista LISP
                       vla_operation_error_obj           ; Para capturar erros de operações VLA
                       modified_block_count              ; Contador
                     )

  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg)
    (IAN:std-error msg
                   (function
                     (lambda ()
                       (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
                       (IAN:restore-osnap old_osmode))))
  )

  ;; --- Inicialização ---
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; Suprime eco de comandos
  (setq old_osmode (IAN:disable-osnap))
  (setq modified_block_count 0)

  (princ "\n>>> ALTERAR ALTURA DE BLOCOS EM SEQUÊNCIA (ALTBLOCOSEQ) <<<")

  ;; --- Etapa 1: Selecionar o Primeiro Bloco ---
  (setq first_block_vla nil)
  (while (not first_block_vla)
    (setq first_block_ename (car (entsel "\nSelecione o PRIMEIRO bloco da sequência: ")))
    (cond
      ((null first_block_ename) ; Usuário pressionou ESC
       (princ "\nSeleção cancelada. Comando encerrado.")
       (setvar "CMDECHO" old_cmdecho)
       (IAN:restore-osnap old_osmode)
       (exit)
      )
      ((setq vla_operation_error_obj (vl-catch-all-apply 'vlax-ename->vla-object (list first_block_ename)))
       (if (vl-catch-all-error-p vla_operation_error_obj)
         (princ (strcat "\n[ABS ERRO] Falha ao obter objeto VLA: " (vl-catch-all-error-message vla_operation_error_obj)))
         (progn ; Objeto VLA obtido com sucesso
           (setq first_block_vla vla_operation_error_obj)
           (if (not (and (vlax-property-available-p first_block_vla 'ObjectName)
                         (= (vla-get-ObjectName first_block_vla) "AcDbBlockReference")))
             (progn
               (princ "\nA entidade selecionada não é um bloco. Tente novamente.")
               (setq first_block_vla nil) ; Reseta para continuar o loop while
             )
             (setq first_block_name (vla-get-EffectiveName first_block_vla)) ; Sucesso
           )
         )
       )
      )
    )
  )
  (princ (strcat "\nBloco inicial selecionado: '" first_block_name "' (Handle: " (vla-get-Handle first_block_vla) ")"))

  ;; --- Etapa 2: Obter Altura de Deslocamento ---
  (setq height_offset (getreal "\nDigite a altura de deslocamento Z para blocos alternados (ex: 0.5): "))
  (if (or (null height_offset) (equal height_offset 0.0 1e-8)) ; Compara com tolerância para zero
    (progn 
      (princ "\nAltura de deslocamento inválida ou zero. Comando cancelado.")
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )

  ;; --- Etapa 3a: Coletar Todas as Instâncias do Mesmo Bloco ---
  (setq all_instances_of_block (IAN:abs-get-block-instances first_block_name))
  (if (or (null all_instances_of_block) (< (length all_instances_of_block) 1)) 
    (progn 
      (princ (strcat "\nNenhuma instância adicional do bloco '" first_block_name "' encontrada (ou apenas a selecionada)."))
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ (strcat "\nEncontradas " (itoa (length all_instances_of_block)) " instâncias totais do bloco '" first_block_name "'."))

  ;; --- Etapa 3b: Ordenar Blocos por Proximidade ---
  (princ "\nOrdenando blocos por proximidade...")
  (setq ordered_block_sequence (IAN:abs-sort-blocks-by-proximity first_block_vla all_instances_of_block))
  
  (if (or (null ordered_block_sequence) (< (length ordered_block_sequence) 2))
    (progn 
      (princ "\nNão foi possível ordenar uma sequência de pelo menos 2 blocos. Verifique os blocos.")
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ (strcat "\n" (itoa (length ordered_block_sequence)) " blocos ordenados na sequência."))

  ;; --- Etapa 4: Aplicar Altura Intercalada ---
  (princ "\nAplicando altura intercalada...")
  (setq index 0)
  (foreach block_vla_in_loop ordered_block_sequence
    (if (= 1 (rem index 2)) ; Aplica ao segundo, quarto, sexto, etc. (índices ímpares)
      (progn
        (setq current_insertion_pt_list (IAN:abs-get-insertion-point-list block_vla_in_loop))
        (if current_insertion_pt_list
          (progn
            (setq new_insertion_pt_list 
                  (list 
                    (car current_insertion_pt_list)
                    (cadr current_insertion_pt_list)
                    (+ (caddr current_insertion_pt_list) height_offset)
                  )
            )
            (setq vla_operation_error_obj 
                  (vl-catch-all-apply 'vla-put-InsertionPoint 
                                      (list block_vla_in_loop (vlax-3d-point new_insertion_pt_list))
                  )
            )
            (if (vl-catch-all-error-p vla_operation_error_obj)
              (princ (strcat "\n  [ABS AVISO] Falha ao mover bloco (Handle: " (vla-get-Handle block_vla_in_loop) 
                             "). Erro: " (vl-catch-all-error-message vla_operation_error_obj)))
              (setq modified_block_count (1+ modified_block_count))
            )
            (princ ".") ; Feedback de progresso
          )
          (princ (strcat "\n  [ABS AVISO] Falha ao obter ponto de inserção do bloco (Handle: " (vla-get-Handle block_vla_in_loop) ") durante a aplicação da altura."))
        )
      )
    )
    (setq index (1+ index))
  )

  ;; --- Etapa 5: Feedback Final ---
  (if (> modified_block_count 0)
    (princ (strcat "\nSucesso! " (itoa modified_block_count) " bloco(s) tiveram a altura modificada."))
    (princ "\nNenhum bloco teve a altura modificada (verifique a sequência ou se havia apenas um bloco)."))

  ;; --- Finalização ---
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
(defun c:APAGAR_BLOCOS_INTERCALADOS_INVERSO ( /
                               *error*                           ; Manipulador de erro local
                               old_cmdecho old_osmode            ; Para restaurar CMDECHO e OSMODE
                               first_block_ename first_block_vla first_block_name ; Bloco inicial
                               all_instances_of_block            ; Lista de todos os blocos com o mesmo nome
                               ordered_block_sequence            ; Blocos ordenados por proximidade
                               block_vla_in_loop index           ; Para iteração
                               vla_operation_error_obj           ; Para capturar erros de operações VLA
                               deleted_block_count               ; Contador
                             )

  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg)
    (IAN:std-error msg
                   (function
                     (lambda ()
                       (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
                       (IAN:restore-osnap old_osmode))))
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
      ((setq vla_operation_error_obj (vl-catch-all-apply 'vlax-ename->vla-object (list first_block_ename)))
       (if (vl-catch-all-error-p vla_operation_error_obj)
         (princ (strcat "\n[DELBLOCINV ERRO] Falha ao obter objeto VLA: " (vl-catch-all-error-message vla_operation_error_obj)))
         (progn 
           (setq first_block_vla vla_operation_error_obj)
           (if (not (and (vlax-property-available-p first_block_vla 'ObjectName)
                         (= (vla-get-ObjectName first_block_vla) "AcDbBlockReference")))
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
  (princ (strcat "\nBloco inicial selecionado: '" first_block_name "' (Handle: " (vla-get-Handle first_block_vla) ")"))

  ;; --- Etapa 2: Coletar Todas as Instâncias do Mesmo Bloco ---
  ;; Reutiliza a função auxiliar IAN:abs-get-block-instances
  (setq all_instances_of_block (IAN:abs-get-block-instances first_block_name))
  (if (or (null all_instances_of_block) (< (length all_instances_of_block) 1)) 
    (progn 
      (princ (strcat "\nNenhuma instância adicional do bloco '" first_block_name "' encontrada (ou apenas a selecionada)."))
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ (strcat "\nEncontradas " (itoa (length all_instances_of_block)) " instâncias totais do bloco '" first_block_name "'."))

  ;; --- Etapa 3: Ordenar Blocos por Proximidade ---
  ;; Reutiliza a função auxiliar IAN:abs-sort-blocks-by-proximity
  (princ "\nOrdenando blocos por proximidade...")
  (setq ordered_block_sequence (IAN:abs-sort-blocks-by-proximity first_block_vla all_instances_of_block))
  
  (if (null ordered_block_sequence) ; Não precisa verificar < 2 aqui, pois até 1 bloco pode ser apagado se for o primeiro.
    (progn 
      (princ "\nNão foi possível ordenar a sequência de blocos. Verifique os blocos.")
      (setvar "CMDECHO" old_cmdecho)
      (IAN:restore-osnap old_osmode)
      (exit)
    )
  )
  (princ (strcat "\n" (itoa (length ordered_block_sequence)) " blocos ordenados na sequência."))

  ;; --- Etapa 4: Aplicar Remoção Intercalada (Inversa) ---
  (princ "\nRemovendo blocos de forma intercalada (primeiro sim, segundo não...)...")
  (setq index 0)
  (foreach block_vla_in_loop ordered_block_sequence
    (if (= 0 (rem index 2)) ; Aplica ao primeiro, terceiro, quinto, etc. (índices pares: 0, 2, 4...)
      (progn
        (setq vla_operation_error_obj (vl-catch-all-apply 'vla-Delete (list block_vla_in_loop)))
        (if (vl-catch-all-error-p vla_operation_error_obj)
          (princ (strcat "\n  [DELBLOCINV AVISO] Falha ao apagar bloco (Handle: " (vla-get-Handle block_vla_in_loop) 
                         "). Erro: " (vl-catch-all-error-message vla_operation_error_obj)))
          (setq deleted_block_count (1+ deleted_block_count))
        )
        (princ ".") ; Feedback de progresso
      )
    )
    (setq index (1+ index))
  )

  ;; --- Etapa 5: Feedback Final ---
  (if (> deleted_block_count 0)
    (princ (strcat "\nSucesso! " (itoa deleted_block_count) " bloco(s) '" first_block_name "' foram apagados."))
    (princ "\nNenhum bloco foi apagado (verifique a sequência)."))

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
(defun c:EXPORTAR_DADOS_BLOCOS ( /
                                *error* old_cmdecho old_osmode
                                ss_blocks selection_count i ent_name vla_block
                                pt_variant pt_safearray pt_list
                                x y z rotation_rad
                                file_path f
                                export_count
                              )

  ;; --- Manipulador de Erro Local ---
  (defun *error* (msg)
    (IAN:std-error msg
                   (function
                     (lambda ()
                       (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
                       (setq f (IAN:close-handle f))
                       (IAN:restore-osnap old_osmode))))
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
    (progn (princ "\nNenhum bloco selecionado. Comando cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit))
  )
  (setq selection_count (sslength ss_blocks))
  (princ (strcat "\n" (itoa selection_count) " bloco(s) selecionado(s) para exportação."))

  ;; --- Etapa 2: Obter Arquivo de Saída ---
  (setq file_path (getfiled "Salvar arquivo de dados dos blocos TXT" "" "txt" 1)) ; 1 = Salvar (pode sobrescrever)
  (if (null file_path)
    (progn (princ "\nNenhum arquivo selecionado para salvar. Comando cancelado.") (setvar "CMDECHO" old_cmdecho) (IAN:restore-osnap old_osmode) (exit))
  )

  ;; --- Etapa 3: Processar Blocos e Escrever no Arquivo ---
  (setq f (IAN:safe-open file_path "w"))
  (if (not f)
    (progn
      (princ (strcat "\n[EXDBLK ERRO] Não foi possível criar/abrir o arquivo: " file_path))
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
            (rtos x 2 8) "," 
            (rtos y 2 8) "," 
            (rtos z 2 8) "," 
            (rtos rotation_rad 2 8) ; Ângulo em radianos com 8 casas decimais
          ) 
          f
        )
        (setq export_count (1+ export_count))
        (princ ".")
      )
      (princ (strcat "\n  [EXDBLK AVISO] Falha ao processar um bloco selecionado: " 
                     (vl-catch-all-error-message vla_block)))
    )
    (setq i (1+ i))
  )
  (setq f (IAN:close-handle f))

  ;; --- Feedback Final ---
  (if (> export_count 0)
    (princ (strcat "\nSucesso! Dados de " (itoa export_count) " bloco(s) exportados para " file_path "."))
    (princ "\nNenhum dado de bloco foi exportado.")
  )

  ;; --- Finalização ---
  (setvar "CMDECHO" old_cmdecho)
  (IAN:restore-osnap old_osmode)
  (princ)
)
(defun c:EXDBLK () (c:EXPORTAR_DADOS_BLOCOS)) ; Alias curto


;;;; ----------------------------------------------------------------------;;;;
;;; Mensagem de Carregamento Final                                         ;;;
;;;; ----------------------------------------------------------------------;;;;
(princ "\nComandos LSP contenções carregados:")
(princ "\n  COMANDOS")
(princ "\n  GERAR_PONTOS_REGUA (GPR)")
(princ "\n  COLOCAR_BLOCO_EM_PONTOS_MEDIOS (CBPM)")
(princ "\n  ALTBLOCOSEQ (ABS)") 
(princ "\n  APAGAR_BLOCOS_INTERCALADOS_INVERSO (DELBLOCINV)") 
(princ "\nDigite o nome do comando ou seu alias (entre parênteses) para usar.")
(princ)

