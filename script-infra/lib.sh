#!/usr/bin/env bash
LIB_DIR="$(cd "$(dirname "${BASH_SOURCE}")" && pwd)"
CONFIG_FILE="${LIB_DIR}/.env"

if [ -f "${CONFIG_FILE}" ]; then
    source "${CONFIG_FILE}"
else
    echo "ERRO CRÍTICO: Arquivo config.env não encontrado em ${CONFIG_FILE}" >&2
    exit 1
fi

GOTIFY_URL="http://${GOTIFY_IP}/message"

log_msg() {
    local mensagem="$1"
    local log_file="${2:-sistema.log}"
    local data_hora=$(date '+%d/%m/%Y %H:%M:%S')

    echo "[${data_hora}] ${mensagem}" >> "${LOG_DIR}/${log_file}"
}

create_log_dir() {
    # O '!' significa "NÃO". Ou seja: "Se NÃO existir o diretório..."
    if [ ! -d "${LOG_DIR}" ]; then
        if mkdir -p "${LOG_DIR}"; then
            log_msg "SUCESSO: Diretório de log criado em ${LOG_DIR}."
        else
            echo "ERRO CRÍTICO: Falha ao criar ${LOG_DIR}. Verifique as permissões." >&2
            return 1
        fi
    fi
}

enviar_gotify() {
    local titulo="$1"
    local mensagem="$2"
    local prioridade="${3:-5}" # Se não informar a prioridade, usa 5 como padrão
    local data_hora=$(date '+%d/%m/%Y %H:%M:%S')
    local arquivo_log="gotify.log"

    # Registra a tentativa no arquivo de log
    log_msg "Enviando notificação: '${titulo}' - '${mensagem}'" "${arquivo_log}"

    # Executa o curl em modo silencioso (-s) e captura o código HTTP
    local http_code=$(curl -s -o /dev/null -w "%{http_code}" "$GOTIFY_URL" \
        -H "X-Gotify-Key: $GOTIFY_TOKEN" \
        -F "title=$titulo" \
        -F "message=$mensagem" \
        -F "priority=$prioridade")

    # Verifica se o servidor Gotify respondeu com sucesso (HTTP 200)
    if [ "${http_code}" -eq 200 ]; then
        log_msg "SUCESSO: Mensagem entregue ao Gotify." "${arquivo_log}"
        return 0
    else
        log_msg "ERRO: Falha ao enviar ao Gotify (Código HTTP: ${http_code})." "${arquivo_log}"
        return 1
    fi
}