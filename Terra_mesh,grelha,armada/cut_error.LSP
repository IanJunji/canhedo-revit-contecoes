; --- Início do Código Definitivo X ---

(defun c:X (/ *error* obj_alvo_sel obj_alvo_ent ponto_ref_ucs ponto_ref_wcs vla_obj ponto_quebra_wcs ponto_quebra_ucs)

  ; Define uma função de erro local para garantir que o comando seja cancelado de forma limpa
  ; se o usuário pressionar a tecla ESC em qualquer etapa.
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nErro: " msg))
    )
    (princ) ; Finaliza de forma limpa
  )

  (vl-load-com) ; Garante que as funções avançadas (vlax, vla, vl) estejam disponíveis.
  (princ "\n>> Comando X (v4 - Definitivo) ativado.")

  ; --- PASSO 1: SELECIONAR O OBJETO A SER QUEBRADO ---
  (setq obj_alvo_sel (entsel "\n[1/2] Selecione o objeto que deseja QUEBRAR: "))
  
  (if obj_alvo_sel
    (progn
      (setq obj_alvo_ent (car obj_alvo_sel)) ; Pega a entidade

      ; --- PASSO 2: SELECIONAR O PONTO DE REFERÊNCIA ---
      (setq ponto_ref_ucs (getpoint "\n[2/2] Selecione o ponto de referência para a quebra: "))
      
      (if ponto_ref_ucs
        (progn
          ; --- ETAPA DE CÁLCULO E TRADUÇÃO ---

          ; 1. Traduz o ponto do clique do UCS (1) do usuário para o WCS (0) global.
          (setq ponto_ref_wcs (trans ponto_ref_ucs 1 0))

          ; 2. Converte a entidade em um objeto VLA e calcula o ponto mais próximo (em WCS).
          (setq vla_obj (vlax-ename->vla-object obj_alvo_ent))
          (setq ponto_quebra_wcs (vlax-curve-getClosestPointTo vla_obj ponto_ref_wcs))

          (if ponto_quebra_wcs
            (progn
              ; 3. Traduz o ponto calculado de volta do WCS (0) para o UCS (1) do usuário.
              (setq ponto_quebra_ucs (trans ponto_quebra_wcs 0 1))
              
              ; --- ETAPA DE EXECUÇÃO (A MUDANÇA CRÍTICA) ---
              ; Usamos 'vl-cmdf', que é mais robusto, e o símbolo "@"
              ; para indicar "o último ponto inserido".
              (vl-cmdf "_.break" obj_alvo_ent "_f" ponto_quebra_ucs "@")

              (princ "\nObjeto quebrado com sucesso!")
            )
            (princ "\nERRO: Não foi possível calcular um ponto válido no objeto selecionado.")
          )
        )
        (princ "\nNenhum ponto de referência selecionado. Comando cancelado.")
      )
    )
    (princ "\nNenhum objeto selecionado. Comando cancelado.")
  )
  
  (princ) ; Finaliza o comando principal de forma limpa.
)

; --- Fim do Código Definitivo XBRK ---