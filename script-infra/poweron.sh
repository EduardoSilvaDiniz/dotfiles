#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/lib.sh"

create_log_dir

log_msg "Iniciando Servidor" "energia.log"

sleep 60

enviar_gotify "Power On" "Servidor Iniciado" 5