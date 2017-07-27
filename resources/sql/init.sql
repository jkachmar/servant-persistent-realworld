--------------------------------------------------------------------------------
-- init.sql: Initialize servant-persistent-realworld's database
--------------------------------------------------------------------------------

CREATE DATABASE srw_db;
CREATE USER     srw_user;

GRANT ALL PRIVILEGES ON DATABASE srw_db TO srw_user;
