;;; -----------------------------------------------------------------------------------
;;; 3DFaceParaTXT.lsp
;;; 
;;; Descrição:
;;; Esta rotina solicita ao usuário que selecione objetos 3DFACE.
;;; Em seguida, extrai as coordenadas X, Y e Z de cada um dos quatro vértices
;;; de cada 3DFACE selecionada e as exporta para um arquivo de texto (.txt).
;;; Cada coordenada de vértice é escrita em uma nova linha no formato "X,Y,Z".
;;;
;;; Uso:
;;; 1. Carregue o arquivo LISP no AutoCAD usando o comando APPLOAD.
;;; 2. Digite o comando "SurfaceText" na linha de comando e pressione Enter.
;;; 3. Siga as instruções na linha de comando.
;;; -----------------------------------------------------------------------------------

(defun C:SurfaceText (/ ss total i ename edata p1 p2 p3 p4 file_path f)
  ;; --- Início da Função Principal ---

  ;; Solicita ao usuário que selecione os objetos, filtrando apenas 3DFaces
  (prompt "\n>> Selecione as 3DFaces para exportar as coordenadas: ")
  (setq ss (ssget '((0 . "3DFACE"))))

  ;; Verifica se alguma 3DFACE foi selecionada
  (if ss
    (progn ; Se a seleção não for nula, executa este bloco

      ;; Solicita ao usuário o local e o nome para salvar o arquivo .txt
      (setq file_path (getfiled "Salvar arquivo de coordenadas" "" "txt" 1))

      ;; Verifica se o usuário selecionou um local para salvar (não cancelou)
      (if file_path
        (progn ; Se um caminho de arquivo foi fornecido, executa este bloco

          ;; Tenta abrir o arquivo no modo de escrita ("w" = write)
          (setq f (open file_path "w"))

          ;; Verifica se o arquivo foi aberto com sucesso
          (if f
            (progn ; Se o arquivo foi aberto, executa este bloco
              
              (princ "\nProcessando entidades... ")
              
              ;; Inicializa o contador e obtém o número total de itens na seleção
              (setq i 0)
              (setq total (sslength ss))

              ;; Loop que se repete para cada objeto na seleção
              (repeat total
                ;; Obtém o nome da entidade (um ponteiro interno do AutoCAD)
                (setq ename (ssname ss i))
                ;; Obtém a lista de dados da entidade (lista de códigos DXF)
                (setq edata (entget ename))

                ;; Extrai as coordenadas de cada vértice da 3DFACE
                ;; (cdr (assoc CODIGO edata)) extrai o valor associado ao código DXF
                ;; Código 10: Primeiro vértice
                ;; Código 11: Segundo vértice
                ;; Código 12: Terceiro vértice
                ;; Código 13: Quarto vértice (pode ser igual ao terceiro para faces triangulares)
                (setq p1 (cdr (assoc 10 edata)))
                (setq p2 (cdr (assoc 11 edata)))
                (setq p3 (cdr (assoc 12 edata)))
                (setq p4 (cdr (assoc 13 edata)))
                
                ;; Escreve cada ponto no arquivo, formatando-o como "X,Y,Z"
                ;; A função 'write-line' adiciona uma quebra de linha automaticamente
                (write-line (strcat (rtos (car p1)) "," (rtos (cadr p1)) "," (rtos (caddr p1))) f)
                (write-line (strcat (rtos (car p2)) "," (rtos (cadr p2)) "," (rtos (caddr p2))) f)
                (write-line (strcat (rtos (car p3)) "," (rtos (cadr p3)) "," (rtos (caddr p3))) f)
                (write-line (strcat (rtos (car p4)) "," (rtos (cadr p4)) "," (rtos (caddr p4))) f)
                
                ;; Incrementa o contador para ir para o próximo objeto na seleção
                (setq i (1+ i))
              ) ; Fim do loop 'repeat'

              ;; Fecha o arquivo. ESSENCIAL para salvar o conteúdo corretamente!
              (close f)
              
              ;; Mensagem de sucesso para o usuário
              (princ (strcat "\nConcluído! " (itoa total) " 3DFaces processadas."))
              (princ (strcat "\nCoordenadas salvas em: " file_path))

            ) ; Fim do progn (se o arquivo foi aberto)
            (princ "\nERRO: Não foi possível criar ou abrir o arquivo para escrita.")
          ) ; Fim do if (verificação de abertura do arquivo)
        ) ; Fim do progn (se um caminho de arquivo foi fornecido)
        (princ "\nOperação cancelada pelo usuário (diálogo de arquivo).")
      ) ; Fim do if (verificação do caminho do arquivo)
    ) ; Fim do progn (se a seleção não for nula)
    (princ "\nNenhuma 3DFACE selecionada.") ; Mensagem se nada foi selecionado
  ) ; Fim do if (verificação da seleção)

  (princ) ; 'Limpa' a linha de comando, retornando um valor nulo de forma silenciosa
) ; --- Fim da Função Principal ---
(defun C:ST () (c:SurfaceText))


(princ "\nLSP 'SurfaceText' carregada. Digite SurfaceText para executar.")
(princ)