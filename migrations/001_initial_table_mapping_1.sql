BEGIN;

SET LOCAL search_path TO public;

CREATE TABLE mappings_owners (
    owner_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    owner_first_name TEXT,
    owner_last_name TEXT,
    owner_name TEXT,
    owner_type TEXT NOT NULL CHECK (owner_type IN ('human', 'ghost')),
    owner_status TEXT CHECK (owner_status IN ('active', 'retired')),
    change_date TIMESTAMPTZ(0) NOT NULL
);

CREATE TABLE mappings_teams (
    team_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    owner_id INTEGER NOT NULL REFERENCES mappings_owners (owner_id),
    season INTEGER NOT NULL,
    season_team_id INTEGER NOT NULL,
    team_name TEXT NOT NULL,
    team_logo TEXT,
    change_date TIMESTAMPTZ(0) NOT NULL
);

COMMENT ON COLUMN mappings_teams.team_logo IS 'S3 object key';

CREATE FUNCTION set_change_date()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.change_date := clock_timestamp();
    ELSIF (to_jsonb(NEW) - 'change_date') IS DISTINCT FROM
          (to_jsonb(OLD) - 'change_date') THEN
        NEW.change_date := clock_timestamp();
    ELSE
        NEW.change_date := OLD.change_date;
    END IF;

    RETURN NEW;
END;
$$;

CREATE TRIGGER set_change_date
BEFORE INSERT OR UPDATE ON mappings_owners
FOR EACH ROW EXECUTE FUNCTION set_change_date();

CREATE TRIGGER set_change_date
BEFORE INSERT OR UPDATE ON mappings_teams
FOR EACH ROW EXECUTE FUNCTION set_change_date();

COMMIT;
