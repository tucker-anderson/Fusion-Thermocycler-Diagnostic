CREATE USER tc_admin WITH PASSWORD 'tc_root_password';
CREATE USER tc_shiny_user WITH PASSWORD 'tc_shiny_pass';
CREATE USER tc_eng_user WITH PASSWORD 'tc_eng_pass';

CREATE DATABASE tc;
\c tc

CREATE TABLE IF NOT EXISTS public.panther_info (
    id SERIAL PRIMARY KEY,
    panther_sn VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS public.tc_info(
    id SERIAL PRIMARY KEY,
    tc_sn VARCHAR(10),
    tc_pn VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS public.scan_files(
    md5 VARCHAR(32) PRIMARY KEY,
    file_type VARCHAR(4),
    file BYTEA
);

GRANT ALL PRIVILEGES ON DATABASE tc TO tc_admin;
GRANT INSERT ON ALL TABLES IN SCHEMA public TO tc_shiny_user;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO tc_eng_user;

-- tests
-- INSERT INTO scan_files VALUES ('test@test.com', 'Test*123');
-- INSERT INTO products (title, price, category) VALUES ('Truco', 9.90, 13);