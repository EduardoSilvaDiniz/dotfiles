#!/usr/bin/env bash

while true; do
    clear
    echo "====== MENU DE PLAYBOOKS ANSIBLE ======"
    echo

    # Lista apenas arquivos .yml ou .yaml
    files=( *.yml *.yaml )

    # se não achar nenhum arquivo, avisa
    if [[ ${#files[@]} -eq 0 ]]; then
        echo "Nenhum playbook (.yml/.yaml) encontrado no diretório atual."
        exit 1
    fi

    # Mostrar menu numerado
    for i in "${!files[@]}"; do
        echo "[$((i+1))] ${files[$i]}"
    done

    echo "[0] Sair"
    echo
    read -p "Escolha um playbook para executar: " choice

    # Sair
    if [[ "$choice" -eq 0 ]]; then
        echo "Encerrando..."
        exit 0
    fi

    # Validar opção
    if ! [[ "$choice" =~ ^[0-9]+$ ]] || (( choice < 1 || choice > ${#files[@]} )); then
        echo "Opção inválida!"
        sleep 1
        continue
    fi

    selected="${files[$((choice-1))]}"

    echo
    echo "Executando playbook: $selected"
    echo "--------------------------------"
    echo

    ansible-playbook "$selected" -K

    echo
    echo "--------------------------------"
    echo "Playbook concluído. Pressione ENTER para voltar ao menu."
    read
done

