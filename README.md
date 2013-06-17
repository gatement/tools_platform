tools_platform
=============

## Description
A platform that runs multiform tools.

## Prerequisites
* [Erlang R16B](http://www.erlang.org/download.html)
* [Yaws](http://yaws.hyber.org/)

## Database
* Mnesia

## Build
* Use `rebar` to build the source, the `rebar` is located at `priv/script/rebar`. Recommend copying `rebar` to your path or add folder `priv/script/rebar` to your path.
* Go to each sub application folder and run the command `rebar get-deps compile` 

## Configure
Move file `priv/conf/local.config.sample` to root with name `local.config` and update the settings in it to proper values.

## Setup/update database
Startup with interactive mode, then execute "db_schema:up().". 

## Run
Please take the file `run.bat` as an example of starting up the service. 

## Deployment
* Delete tables by "mnesia:delete_table(table_name).": dev_device, dev_session, dev_status, dev_data
* "db_schema:up()."

