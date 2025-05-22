## Linha de Comandos

| Combinação de Teclas                | Descrição                                   |
|-------------------------------------|---------------------------------------------|
| tmux                                | inicia o tmux e cria uma sessão             |
| tmux ls                             | listar todas as sessoẽs                     |
| tmux new -s {NomeDaSessão}          | inicia uma sessão com nome especifico       |
| tmux new -s {NomeDaSessão} -d       | cria uma sessão detached                    |
| tmux kill-session -t {NomeDaSessão} | mata uma sessão do nome passado             |
| tmux kill-server                    | mata todas as sessoẽs                       |


## Combinação de Teclas
MASTRE = CTRL(^) + b
percione a tecla mestre e depois a proxima tecla, não pode ser tudo junto, ERRADO - ctrl+b+c, Certo - Ctrl+b c

| Combinação de Teclas | Descrição |
|----------------------|-----------|
| MASTRE + d | detached na sessão  |
| MASTRE + $ | renomear a sessão   |
| MASTRE + w | modo interativo     |
| MASTRE + t | mostra horas        |


### Navegação Entre Janelas
| Combinação de Teclas | Descrição |
|----------------------|-----------|
| MASTRE + c   | nova janela       |
| MASTRE + n   | proxima janela    |
| MASTRE + p   | janela anterior   |
| MASTRE + 0-9 | janela especifica |

### Navegação Entre Janelas Horizontal/Vertical
| Combinação de Teclas | Descrição |
|----------------------|-----------|
| MASTRE + "     | Modo Horizontal |
| MASTRE + %     | modo vertical   |
| MASTRE + z     | foco na janela  |
| MASTRE + setas | troca de janela |
