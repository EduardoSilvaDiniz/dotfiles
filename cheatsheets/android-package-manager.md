# Tabela de Apps
| nome do aplicativo | nome do pacote                              |
|--------------------|---------------------------------------------|
| Play Store         | com.android.vending                         |
| Google Chrome      | com.android.chrome                          |
| Galeria            | com.android.calendar                        |
| Google Search      | com.google.android.googlequicksearchbox     |
| Clima              | com.miui.weather2                           |
| Bússola            | com.miui.compass                            |
| Galeria            | com.miui.gallery                            |
| Radio.fm           | com.miui.fm                                 |
| Galeria Miui       | com.xiaomi.calendar                         |

## Procurando Apps
pesquise o nome do app na playstore, copie o id da url, exemplo...
https://play.google.com/store/apps/details?id=com.microsoft.emmx  -- microsoft edge

o id da url é o nome do pacote do app (com.microsoft.emmx)

### Comandos adb
utilize `adb devices` para verificar se o seu celular está conectado (deporação precisa está ativada)
na distro Fedora, é necessario inicia o servidor adb como root `sudo adb start-server`
caso já esteja ligado, use `adb kill-server` e depois inicie como root
para se conectar ao celular, use `adb shell`

#### Gerenciador de Pacotes pm
| Comando                  | Descrição                                   |
|--------------------------|---------------------------------------------|
| Desativar para o Usuario | `pm disable-user --user 0 NomeDoPacote`     |
| Ativar                   | `pm enable NomeDoPacote`                    |
| Remover                  | `pm uninstall NomeDoPacote`                 |
| Instalar                 | `pm install NomeDoPacote`                   |