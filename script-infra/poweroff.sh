#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/lib.sh"

create_log_dir

log_msg "Desligando Servidor" "energia.log"
enviar_gotify "Power Off" "Servidor será desligado" 5

shutdown -P now