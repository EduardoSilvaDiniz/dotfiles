#!/usr/bin/env bash
set -e

echo "=== Instalando dependências ==="
apt update
apt install -y wget libopengl0 libglx0 libegl1

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

echo "=== Concluído! ==="
systemctl status openrgb.service --no-pager
