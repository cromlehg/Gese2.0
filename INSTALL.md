### Требования
* Сервер под управлением Ubuntu 16.04 LTS
* Аккаунт SendGrid

### Зависимости
* SBT (https://www.scala-sbt.org/download.html)
* JDK 1.8
* Scala 
* MySQL
* Git

### Установка
* Склонировать репозиторий
* Создать БД 
* Создать таблицы путем выполнения скрипта - dev/sql/create_tables.sql
* Создать файл conf/application.conf по аналогии с conf/example_application.conf
* В файле conf/application.conf указать уникальный секретный ключ (https://www.playframework.com/documentation/2.6.x/ApplicationSecret)
* Выполнить sbt dist
* После выполнения распаковать zip файл с созданным дистрибутивом
* Запустить bin/gese &

