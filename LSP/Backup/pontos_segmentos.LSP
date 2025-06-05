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
                               tolerancia start_pt_entidade_catch) 
  (defun *error* (msg)
    (if f (progn (close f) (setq f nil)))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\nErro (GERAR_PONTOS_REGUA): " msg)))
    (princ)
  )
  (setq tolerancia 0.001) (setq comprimento_regua nil)
  (princ "\n>>> GERADOR DE PONTOS COM RÉGUA (GPR) <<<")
  (princ "\nSelecione as entidades alvo (linhas, polilinhas, arcos, splines): ")
  (setq ss_linhas_alvo (ssget '((0 . "LINE,LWPOLYLINE,ARC,SPLINE"))))

  (if (null ss_linhas_alvo) (progn (princ "\nNenhuma entidade selecionada. Comando cancelado.") (exit)))

  (while (or (null comprimento_regua) (<= comprimento_regua 0.0))
    (setq comprimento_regua (getdist "\nDigite o comprimento da régua/segmento: "))
    (if (or (null comprimento_regua) (<= comprimento_regua 0.0)) (princ "\nComprimento inválido. Deve ser positivo."))
  )
  (setq arquivo_saida (getfiled "Salvar arquivo de coordenadas TXT" "" "txt" 1))
  (if (null arquivo_saida) (progn (princ "\nNenhum arquivo selecionado. Comando cancelado.") (exit)))

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
      (setq f (open arquivo_saida "w"))
      (if f
        (progn
          (princ (strcat "\n\nEscrevendo " (itoa (length lista_todos_os_pontos)) " pontos em: " arquivo_saida))
          (foreach pt lista_todos_os_pontos
            (write-line (strcat (rtos (car pt) 2 8) "," (rtos (cadr pt) 2 8) "," (rtos (caddr pt) 2 8)) f)
          )
          (close f) (setq f nil)
          (princ "\nSucesso! Arquivo salvo.")
        )
        (princ (strcat "\n[GPR ERRO] Falha ao abrir arquivo para escrita: " arquivo_saida))
      )
    )
    (princ "\n\nNenhum ponto gerado para salvar.")
  )
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
                                        selecao_valida vla_bloco_inserido_raw)
  (defun *error* (msg)
    (if f (progn (close f) (setq f nil)))
    (if old_clayer (setvar "CLAYER" old_clayer))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\nErro (COLOCAR_BLOCO_EM_PONTOS_MEDIOS): " msg)))
    (princ)
  )
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
    (progn (princ "\nArquivo não selecionado/encontrado. Comando cancelado.") (exit))
  )
  (setq f (open arquivo_txt "r"))
  (if (not f) (progn (princ (strcat "\n[CBPM ERRO] Falha ao abrir arquivo: " arquivo_txt)) (exit)))
  
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
  (close f) (setq f nil)
  (setq lista_pontos_lidos (reverse lista_pontos_lidos)) 

  (if (< (length lista_pontos_lidos) 2)
    (progn (princ "\n[CBPM ERRO] Menos de 2 pontos válidos lidos. Comando cancelado.") (exit))
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
    (progn (princ "\n[CBPM ERRO] Nenhum segmento válido para pontos médios. Verifique o arquivo.") (exit))
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
      (progn (princ "\nSeleção cancelada. Comando encerrado.") (exit))
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
  (princ) 
)
(defun c:CBPM () (c:COLOCAR_BLOCO_EM_PONTOS_MEDIOS)) 

;;;; ----------------------------------------------------------------------;;;;
;;; Mensagem de Carregamento Final                                         ;;;
;;;; ----------------------------------------------------------------------;;;;
(princ "\nComandos IAN LISP Utilities carregados:")
(princ "\n  GERAR_PONTOS_REGUA (GPR)")
(princ "\n  COLOCAR_BLOCO_EM_PONTOS_MEDIOS (CBPM)")
(princ "\nDigite o nome do comando ou seu alias (entre parênteses) para usar.")
(princ)