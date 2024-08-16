GRANT CONNECT ON DATABASE flexiprocity TO api, meddler;

CREATE FUNCTION public.get_facebook_id() RETURNS text
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT current_setting('jwt.claims.facebookUserId', true);
  END;

CREATE FUNCTION public.get_google_field(fieldname text) RETURNS text
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT current_setting('jwt.claims.google', true)::jsonb->>fieldname;
  END;

CREATE FUNCTION public.get_google_email() RETURNS text
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT get_google_field('email');
  END;

CREATE TYPE public.audience AS ENUM
  ( 'self'
  , 'friends'
  , 'everyone'
  );

-- Postgraphile doesn't support procedures
CREATE TYPE public.unit AS ENUM ('unit');

CREATE TABLE public.privacy_policies
  ( version text PRIMARY KEY
  );

CREATE TABLE public.contacts
  ( contact_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , email_address text NOT NULL UNIQUE
  , blacklist bool NOT NULL DEFAULT FALSE
  , unsub_requested timestamptz
  , unsub_token uuid
  );
GRANT SELECT ON contacts TO meddler;

CREATE OR REPLACE FUNCTION get_or_create_contact_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL UNSAFE
  BEGIN ATOMIC
    WITH verified AS (
      SELECT
        CASE get_google_field('email_verified')
          WHEN 'true' THEN get_google_email()
        END AS email_address
    ), new_ AS (
      INSERT INTO contacts (email_address)
      SELECT email_address FROM verified WHERE email_address IS NOT NULL
      EXCEPT SELECT email_address FROM contacts
      ON CONFLICT DO NOTHING -- can hit if you try to do this twice in quick succession
      RETURNING contact_id
    )
    SELECT COALESCE(new_.contact_id, old_.contact_id) AS contact_id
    FROM verified
    LEFT JOIN contacts old_ ON old_.email_address = verified.email_address
    LEFT JOIN new_ ON (true);
  END;
REVOKE EXECUTE ON FUNCTION get_or_create_contact_id FROM public;
GRANT  EXECUTE ON FUNCTION get_or_create_contact_id TO api;

CREATE TABLE public.users
  ( user_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , facebook_id text UNIQUE
  , google_email text UNIQUE
  , CHECK ((facebook_id IS NOT NULL) <> (google_email IS NOT NULL))
  , privacy_policy_version text REFERENCES privacy_policies(version)
  , name text
  , picture text
  , bio text NOT NULL DEFAULT ''
  , visible_to audience NOT NULL DEFAULT 'self'
  , show_me audience NOT NULL DEFAULT 'everyone'
  , verified_contact_id bigint REFERENCES contacts(contact_id) ON DELETE SET NULL
  , send_email_on_matches bool NOT NULL DEFAULT FALSE
  , created_at timestamptz NOT NULL DEFAULT now()
  );

COMMENT ON COLUMN public.users.show_me
  IS 'stored in the database but applied on the client';

CREATE FUNCTION public.current_user_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT user_id FROM users
    WHERE facebook_id = get_facebook_id()
      OR google_email = get_google_email();
  END;
REVOKE EXECUTE ON FUNCTION current_user_id FROM public;
GRANT  EXECUTE ON FUNCTION current_user_id TO api;

CREATE FUNCTION public.my_user() RETURNS users
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT * FROM users WHERE user_id = current_user_id();
  END;
REVOKE EXECUTE ON FUNCTION my_user FROM public;
GRANT  EXECUTE ON FUNCTION my_user TO api;

CREATE FUNCTION public.update_me
  ( name text = NULL
  , bio text = NULL
  , visible_to audience = NULL
  , show_me audience = NULL
  , privacy_policy_version text = NULL
  , send_email_on_matches bool = NULL
  ) RETURNS users
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL UNSAFE
  BEGIN ATOMIC
    UPDATE users
    SET name = COALESCE(update_me.name, get_google_field('name'), users.name)
      , bio = COALESCE(update_me.bio, users.bio)
      , picture = COALESCE(get_google_field('picture'), users.picture)
      , verified_contact_id =
          COALESCE(
            get_or_create_contact_id()
          , users.verified_contact_id
          )
      , visible_to = COALESCE(update_me.visible_to, users.visible_to)
      , show_me = COALESCE(update_me.show_me, users.show_me)
      , privacy_policy_version =
          COALESCE(update_me.privacy_policy_version, users.privacy_policy_version)
      , send_email_on_matches =
          COALESCE(update_me.send_email_on_matches, users.send_email_on_matches)
    WHERE users.user_id = current_user_id()
    RETURNING *;
  END;
REVOKE EXECUTE ON FUNCTION update_me FROM public;
GRANT  EXECUTE ON FUNCTION update_me TO api;

CREATE FUNCTION public.delete_me() RETURNS unit
  -- SECURITY DEFINER should allow bypassing RLS where it restricts deletion
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  BEGIN ATOMIC
    DELETE FROM users
    WHERE user_id = current_user_id()
    RETURNING 'unit'::unit;
  END;
REVOKE EXECUTE ON FUNCTION update_me FROM public;
GRANT  EXECUTE ON FUNCTION update_me TO api;

CREATE FUNCTION public.get_or_create_user_id() RETURNS bigint
  LANGUAGE plpgsql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  AS $$DECLARE
    ret_user_id bigint;
  BEGIN
    SELECT current_user_id() INTO ret_user_id;
    IF ret_user_id IS NOT NULL THEN
      RETURN ret_user_id;
    END IF;
    INSERT INTO users (facebook_id, google_email)
    SELECT facebook_id, google_email
    FROM (SELECT
        get_facebook_id() AS facebook_id
      , get_google_email() AS google_email
    ) tmp
    WHERE facebook_id IS NOT NULL OR google_email IS NOT NULL;
    RETURN current_user_id();
  END$$;
REVOKE EXECUTE ON FUNCTION get_or_create_user_id FROM public;
GRANT  EXECUTE ON FUNCTION get_or_create_user_id TO api;

CREATE TABLE public.facebook_friends
  ( user_id   bigint REFERENCES users(user_id) ON DELETE CASCADE
  , friend_id bigint REFERENCES users(user_id) ON DELETE CASCADE
  , since     timestamptz NOT NULL DEFAULT now()
  , PRIMARY KEY (user_id, friend_id)
  );

CREATE FUNCTION public.set_facebook_friends(friend_fbids text[]) RETURNS unit
  LANGUAGE sql SECURITY DEFINER
  BEGIN ATOMIC
    WITH friend_ids AS (
      SELECT user_id AS friend_id FROM users WHERE facebook_id = ANY(friend_fbids)
    )
    , deleted AS (
      DELETE FROM facebook_friends
      WHERE user_id = current_user_id()
      AND friend_id <> ALL(SELECT friend_id FROM friend_ids)
    )
    , inserted AS (
      INSERT INTO facebook_friends (user_id, friend_id)
      SELECT current_user_id(), friend_id
      FROM friend_ids
      ON CONFLICT DO NOTHING
    )
    SELECT 'unit'::unit;
  END;
REVOKE EXECUTE ON FUNCTION set_facebook_friends FROM public;
GRANT  EXECUTE ON FUNCTION set_facebook_friends TO api;

CREATE TABLE public.woulds
  ( would_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , name text NOT NULL UNIQUE
  , added_by_id bigint REFERENCES users(user_id) ON DELETE SET NULL
  , is_default boolean NOT NULL DEFAULT false
  , created_at timestamptz NOT NULL DEFAULT now()
  );
INSERT INTO woulds (name, is_default)
VALUES ('Hang out sometime', true), ('Go on a date or something', true);
GRANT SELECT, INSERT, UPDATE(name), DELETE ON woulds TO api;

CREATE POLICY read_all ON woulds FOR SELECT TO api USING (true);
CREATE POLICY write_mine ON woulds FOR ALL TO api
  USING (added_by_id = current_user_id());
ALTER TABLE woulds ENABLE ROW LEVEL SECURITY;

CREATE TABLE public.user_columns
  ( user_id  bigint NOT NULL REFERENCES users(user_id)   ON DELETE CASCADE
  , would_id bigint NOT NULL REFERENCES woulds(would_id) ON DELETE CASCADE
  , order_pos bigint NOT NULL
  , PRIMARY KEY (user_id, would_id)
  , UNIQUE (user_id, would_id, order_pos)
  );

CREATE FUNCTION public.get_my_columns() RETURNS bigint[]
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT
      COALESCE(
          array_agg(uc.would_id ORDER BY uc.order_pos) FILTER (WHERE uc.would_id IS NOT NULL)
        , (SELECT array_agg(w.would_id ORDER BY w.would_id) FROM woulds w WHERE w.is_default)
        )
    FROM user_columns uc
    WHERE uc.user_id = current_user_id();
  END;
REVOKE EXECUTE ON FUNCTION get_my_columns FROM public;
GRANT  EXECUTE ON FUNCTION get_my_columns TO api;

CREATE FUNCTION public.set_my_columns(columns bigint[]) RETURNS unit
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  BEGIN ATOMIC
    WITH deleted AS (
      DELETE FROM user_columns
      WHERE user_id = current_user_id()
      AND would_id <> ALL(columns)
    ), with_order_pos AS (
      SELECT
        current_user_id() AS user_id
      , col AS would_id
      , row_number() OVER () AS order_pos
      FROM unnest(columns) col
    ), inserted AS (
      INSERT INTO user_columns
        (   user_id,     would_id,     order_pos )
      SELECT
        wop.user_id, wop.would_id, wop.order_pos
      FROM with_order_pos wop
      LEFT JOIN user_columns uc USING (user_id, would_id)
      WHERE uc IS NULL
    ), updated AS (
      UPDATE user_columns uc
      SET order_pos = wop.order_pos
      FROM with_order_pos wop
      WHERE uc.user_id = wop.user_id
      AND uc.would_id = wop.would_id
      AND uc.order_pos <> wop.order_pos
    )
    SELECT 'unit'::unit;
  END;
REVOKE EXECUTE ON FUNCTION set_my_columns FROM public;
GRANT  EXECUTE ON FUNCTION set_my_columns TO api;

CREATE TABLE public.user_woulds
  ( user_id bigint  NOT NULL REFERENCES users(user_id)   ON DELETE CASCADE
  , would_id bigint NOT NULL REFERENCES woulds(would_id) ON DELETE CASCADE
  , with_id bigint  NOT NULL REFERENCES users(user_id)   ON DELETE CASCADE
  , PRIMARY KEY (user_id, would_id, with_id)
  , CHECK (user_id <> with_id)
  );

CREATE POLICY only_my_woulds ON user_woulds FOR ALL TO api
  USING (user_id = current_user_id())
  WITH CHECK (user_id = current_user_id());
CREATE POLICY no_delete_matches ON user_woulds AS RESTRICTIVE FOR DELETE TO api
  USING ((with_id, would_id, user_id) NOT IN (SELECT user_id, would_id, with_id FROM user_woulds));
ALTER TABLE user_woulds ENABLE ROW LEVEL SECURITY;

GRANT SELECT, INSERT, DELETE ON user_woulds TO api;

CREATE VIEW public.would_stats AS
  SELECT
    w.would_id
  , w.name
  , w.added_by_id
  , CASE
      WHEN w.is_default
      THEN NULL
      ELSE count(uc.would_id)
    END AS uses
  FROM woulds w
  LEFT JOIN user_columns uc USING (would_id)
  GROUP BY w.would_id;
GRANT SELECT ON would_stats TO api;

CREATE OR REPLACE FUNCTION public.restrict_custom_woulds() RETURNS TRIGGER
  LANGUAGE plpgsql AS $$DECLARE
    this_user_id bigint;
    num_recent bigint;
  BEGIN
    this_user_id := current_user_id();
    IF TG_OP = 'UPDATE' OR TG_OP = 'DELETE' THEN
      IF EXISTS (
          SELECT FROM user_woulds uw
          WHERE would_id = NEW.would_id
          AND user_id <> this_user_id
        ) THEN
        RAISE EXCEPTION 'Cannot rename or delete a column in use';
      END IF;
      IF TG_OP = 'UPDATE' THEN
        RETURN NEW;
      ELSE
        RETURN OLD;
      END IF;
    ELSIF TG_OP = 'INSERT' THEN
      NEW.added_by_id := this_user_id;
      SELECT count(*)
        FROM woulds
        WHERE added_by_id = this_user_id
        AND created_at > now() - interval '3 days'
        INTO STRICT num_recent;
      IF num_recent >= 3 THEN
        RAISE EXCEPTION 'Cannot create more than 3 columns every 3 days';
      END IF;
      RETURN NEW;
    END IF;
  END$$;

CREATE OR REPLACE TRIGGER restrict_custom_woulds BEFORE INSERT OR UPDATE OR DELETE ON woulds
  FOR EACH ROW
  EXECUTE FUNCTION restrict_custom_woulds();

CREATE VIEW public.user_profiles AS
  SELECT
    users.user_id
  , users.facebook_id
  , users.name
  , users.bio
  , users.picture
  , CASE
      WHEN users.user_id = current_user_id() THEN 'self'
      WHEN uf IS DISTINCT FROM NULL THEN 'friends'
      ELSE 'everyone'
    END::audience AS audience
  , greatest(uf.since, fwu.since) AS friends_since
  , users.created_at AS created_at
  , COALESCE(
      array_agg(uw.would_id) FILTER (WHERE uw.would_id IS NOT NULL)
    , '{}'
    ) AS you_would
  , COALESCE(
      array_agg(matches.would_id) FILTER (WHERE matches.would_id IS NOT NULL)
    , '{}'
    ) AS matched_woulds
  FROM users
  LEFT JOIN facebook_friends fwu
    ON fwu.user_id = users.user_id
   AND fwu.friend_id = current_user_id()
  LEFT JOIN facebook_friends uf
    ON uf.user_id = current_user_id()
   AND uf.friend_id = users.user_id
  LEFT JOIN user_woulds uw
    ON uw.user_id = current_user_id()
   AND uw.with_id = users.user_id
  LEFT JOIN user_woulds matches
    ON matches.user_id = users.user_id
   AND matches.would_id = uw.would_id
   AND matches.with_id = current_user_id()
  WHERE users.visible_to = 'everyone'
     OR (users.visible_to = 'friends' AND fwu IS DISTINCT FROM NULL)
     OR users.user_id = current_user_id()
     OR users.user_id IN (
          SELECT w.user_id
          FROM user_woulds w
          WHERE w.user_id = users.user_id
            AND w.with_id = current_user_id()
        )
  GROUP BY users.user_id, fwu.since, uf.*, uf.since;
GRANT SELECT ON user_profiles TO api;

CREATE TABLE public.matches
  ( match_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , name1 text NOT NULL
  , contact_id1 bigint REFERENCES contacts(contact_id) ON DELETE CASCADE
  , name2 text NOT NULL
  , contact_id2 bigint REFERENCES contacts(contact_id) ON DELETE CASCADE
  , would_matches text[] NOT NULL CHECK (would_matches <> '{}')
  );
GRANT SELECT ON matches TO meddler;

CREATE TABLE public.email_sending
  ( email_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , created_at timestamptz NOT NULL DEFAULT now()
  , sending_started timestamptz
  , sending_completed timestamptz
  , CHECK (sending_started IS NOT NULL OR sending_completed IS NULL)
  , sending_cancelled timestamptz
  , CHECK (sending_completed IS NULL OR sending_cancelled IS NULL)
  , errors text[]
  , match_id bigint REFERENCES matches(match_id)
  , unsub_contact_id bigint REFERENCES contacts(contact_id) ON DELETE CASCADE
  , CHECK ((match_id IS NULL) <> (unsub_contact_id IS NULL))
  );
CREATE UNIQUE INDEX only_one_inflight_unsub
  ON email_sending (unsub_contact_id)
  WHERE sending_started IS NULL;
GRANT SELECT, UPDATE(sending_started, sending_completed, sending_cancelled, errors)
  ON email_sending TO meddler;

CREATE OR REPLACE FUNCTION public.trigger_notify() RETURNS TRIGGER
  LANGUAGE plpgsql SECURITY INVOKER AS $$BEGIN
    PERFORM pg_notify(TG_ARGV[0], TG_ARGV[1]);
    RETURN NULL;
  END$$;

CREATE OR REPLACE TRIGGER notify_emails AFTER INSERT ON email_sending
  FOR EACH ROW
  -- can't access NEW from the function call, so can't pass a useful payload
  EXECUTE FUNCTION trigger_notify('email_sending', '');

CREATE OR REPLACE FUNCTION public.queue_match_emails() RETURNS TRIGGER
  LANGUAGE plpgsql SECURITY DEFINER AS $$BEGIN
    WITH matches_to_queue AS (
      SELECT nuw.user_id, array_agg(woulds.name) AS would_names, nuw.with_id
      FROM new_user_woulds nuw
      JOIN user_woulds wu ON nuw.would_id = wu.would_id AND nuw.user_id = wu.with_id
      JOIN woulds ON woulds.would_id = nuw.would_id
      GROUP BY nuw.user_id, nuw.with_id
    ), new_matches AS (
      INSERT INTO matches (name1, contact_id1, name2, contact_id2, would_matches)
      SELECT
          us.name AS name1
        , CASE WHEN us.send_email_on_matches THEN us.verified_contact_id END AS contact_id1
        , them.name AS name2
        , CASE WHEN them.send_email_on_matches THEN them.verified_contact_id END AS contact_id2
        , mq.would_names AS would_matches
      FROM matches_to_queue mq
      JOIN users us ON us.user_id = mq.user_id
      JOIN users them ON them.user_id = mq.with_id
      WHERE us.send_email_on_matches OR them.send_email_on_matches
      RETURNING match_id
    )
    INSERT INTO email_sending (match_id)
    SELECT match_id FROM new_matches
    ;
    RETURN NULL;
  END$$;

CREATE OR REPLACE TRIGGER email_on_matches AFTER INSERT ON user_woulds
  REFERENCING NEW TABLE AS new_user_woulds
  FOR EACH STATEMENT
  EXECUTE FUNCTION queue_match_emails();

CREATE OR REPLACE FUNCTION public.request_unsub(email_address text) RETURNS unit
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL UNSAFE
  BEGIN ATOMIC
    WITH updated_contact AS (
      UPDATE contacts c
      SET unsub_token = gen_random_uuid(), unsub_requested = now()
      WHERE c.email_address = request_unsub.email_address
      AND COALESCE(unsub_requested < now() - interval '1 day', TRUE)
      RETURNING contact_id
    )
    INSERT INTO email_sending (unsub_contact_id)
    SELECT contact_id FROM updated_contact
    RETURNING 'unit'::unit;
  END;
REVOKE EXECUTE ON FUNCTION request_unsub FROM public;
GRANT  EXECUTE ON FUNCTION request_unsub TO api;

CREATE OR REPLACE FUNCTION public.complete_unsub(email_address text, unsub_token uuid) RETURNS unit
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL UNSAFE
  BEGIN ATOMIC
    UPDATE contacts c
    SET blacklist = TRUE
    WHERE c.email_address = complete_unsub.email_address
      AND c.unsub_token = complete_unsub.unsub_token
    RETURNING 'unit'::unit;
  END;
REVOKE EXECUTE ON FUNCTION complete_unsub FROM public;
GRANT  EXECUTE ON FUNCTION complete_unsub TO api;
