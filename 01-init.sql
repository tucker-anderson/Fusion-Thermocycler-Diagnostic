CREATE USER tc_admin WITH PASSWORD 'tc_root_password';
CREATE USER tc_shiny_user WITH PASSWORD 'tc_shiny_pass';
CREATE USER tc_eng_user WITH PASSWORD 'tc_eng_pass';

CREATE DATABASE tc;
\c tc

CREATE TABLE IF NOT EXISTS public.panther_info (
    panther_id INT GENERATED ALWAYS AS IDENTITY,
    panther_sn VARCHAR(10) NOT NULL,
    PRIMARY KEY(panther_id),
    UNIQUE(panther_sn)
);

CREATE TABLE IF NOT EXISTS public.tc_info(
    tc_id INT GENERATED ALWAYS AS IDENTITY,
    tc_sn VARCHAR(10) NOT NULL,
    tc_pn VARCHAR(20) NOT NULL,
    PRIMARY KEY(tc_id),
    UNIQUE(tc_sn, tc_pn)
);


CREATE TABLE IF NOT EXISTS public.scans(
    scan_id INT GENERATED ALWAYS AS IDENTITY,
    panther_id INT NOT NULL,
    tc_id INT NOT NULL,
    md5 VARCHAR(32) NOT NULL,
    is_peek BOOLEAN NOT NULL,
    is_bg BOOLEAN NOT NULL,
    is_pm_bg BOOLEAN NOT NULL,
    barcode_1 VARCHAR(23),
    barcode_2 VARCHAR(23),
    peek_1_fam VARCHAR(5),
    peek_1_hex VARCHAR(5),
    peek_1_rox VARCHAR(5),
    peek_1_647 VARCHAR(5),
    peek_1_677 VARCHAR(5),
    peek_2_fam VARCHAR(5),
    peek_2_hex VARCHAR(5),
    peek_2_rox VARCHAR(5),
    peek_2_647 VARCHAR(5),
    peek_2_677 VARCHAR(5),
    tc_firmware VARCHAR(10),
    start_timestamp TIMESTAMP NOT NULL,
    end_timestamp TIMESTAMP NOT NULL,
    file BYTEA NOT NULL,
    PRIMARY KEY(scan_id),
    UNIQUE(md5),
    CONSTRAINT fk_panther
      FOREIGN KEY(panther_id) 
	  REFERENCES panther_info(panther_id),
    CONSTRAINT fk_tc
      FOREIGN KEY(tc_id) 
	  REFERENCES tc_info(tc_id)
);

CREATE TABLE IF NOT EXISTS public.reports(
    report_id INT GENERATED ALWAYS AS IDENTITY,
    peek_id INT NOT NULL,
    bg_id INT NOT NULL,
    is_dev BOOLEAN NOT NULL,
    barcode_1 VARCHAR(23),
    barcode_2 VARCHAR(23),
    peek_fam VARCHAR(5),
    peek_hex VARCHAR(5),
    peek_rox VARCHAR(5),
    peek_647 VARCHAR(5),
    peek_677 VARCHAR(5),
    bg_max NUMERIC(3, 2) NOT NULL,
    peek_min NUMERIC(3, 2) NOT NULL,
    led_min NUMERIC(3, 2) NOT NULL,
    peek_decay NUMERIC(3, 2) NOT NULL,
    acceptance BOOLEAN NOT NULL,
    app_version VARCHAR NOT NULL,
    report_ts TIMESTAMPTZ NOT NULL,
    -- excel_md5 VARCHAR(32) NOT NULL,
    -- html_md5 VARCHAR(32) NOT NULL,
    excel_file BYTEA NOT NULL,
    html_file BYTEA NOT NULL,
    PRIMARY KEY(report_id),
    -- UNIQUE(excel_md5, html_md5),
    CONSTRAINT fk_peek
      FOREIGN KEY(peek_id) 
	  REFERENCES scans(scan_id),
    CONSTRAINT fk_bg
      FOREIGN KEY(bg_id) 
	  REFERENCES scans(scan_id)
);

SET timezone = 'America/Los_Angeles';

GRANT ALL PRIVILEGES ON DATABASE tc TO tc_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public to tc_admin;

GRANT CONNECT ON DATABASE tc to tc_shiny_user;
GRANT INSERT, SELECT ON ALL TABLES IN SCHEMA public TO tc_shiny_user;
GRANT CONNECT ON DATABASE tc to tc_eng_user;
GRANT INSERT, SELECT, UPDATE ON ALL TABLES IN SCHEMA public TO tc_eng_user;
-- tests
-- INSERT INTO scan_files VALUES ('test@test.com', 'Test*123');
-- INSERT INTO products (title, price, category) VALUES ('Truco', 9.90, 13);