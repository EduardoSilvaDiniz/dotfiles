#!/usr/bin/env bash
set -e

echo "=== Instalando dependências ==="
apt update
apt install -y wget libopengl0 libglx0 libegl1 i2c-tools

echo "=== Configurando módulos I2C ==="

touch /etc/modules

for module in i2c-dev i2c-i801; do
    if ! grep -qxF "$module" /etc/modules; then
        echo "$module" >> /etc/modules
    fi
done

modprobe i2c-dev
modprobe i2c-i801

echo "=== Baixando OpenRGB ==="

cd /root

wget -O openrgb.AppImage \
"https://codeberg.org/OpenRGB/OpenRGB/releases/download/release_1.1/OpenRGB_1.1_x86_64_8e16de2.AppImage"

chmod +x /root/openrgb.AppImage

echo "=== Criando serviço systemd ==="

cat > /etc/systemd/system/openrgb.service <<'EOF'
[Unit]
Description=Turn off RGB using OpenRGB
After=multi-user.target

[Service]
Type=oneshot
ExecStart=/root/openrgb.AppImage --device 0 --mode off
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
EOF

echo "=== Habilitando serviço ==="

systemctl daemon-reload
systemctl enable openrgb.service
systemctl start openrgb.service

echo "=== Instalação concluída ==="
systemctl status openrgb.service --no-pager

echo
echo "=========================================="
echo " DICA PARA PROCESSADORES AMD"
echo "=========================================="
echo
echo "Se estiver usando AMD, edite /etc/modules:"
echo
echo "vim /etc/modules"
echo
echo "Troque:"
echo "i2c-i801"
echo
echo "Por:"
echo "i2c-piix4"
echo
echo "Mantenha o módulo i2c-dev."
echo "Depois reinicie o Proxmox."
echo
echo "=========================================="
