GRANT CONNECT ON DATABASE flexiprocity TO api;

CREATE FUNCTION public.get_facebook_id() RETURNS text
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT current_setting('jwt.claims.facebookUserId', true);
  END;

CREATE TYPE public.audience AS ENUM
  ( 'self'
  , 'friends'
  , 'everyone'
  );

-- Postgraphile doesn't support procedures
CREATE TYPE public.unit AS ENUM ('unit');

CREATE TABLE public.users
  ( user_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , facebook_id text UNIQUE NOT NULL
  , name text
  , bio text NOT NULL DEFAULT ''
  , visible_to audience NOT NULL DEFAULT 'friends'
  , created_at timestamptz NOT NULL DEFAULT now()
  );

CREATE FUNCTION public.current_user_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  BEGIN ATOMIC
    SELECT user_id FROM users
    WHERE facebook_id = get_facebook_id();
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

CREATE FUNCTION public.update_me(name text, bio text, visible_to audience) RETURNS users
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  BEGIN ATOMIC
    UPDATE users
    SET name = COALESCE(update_me.name, users.name)
      , bio = COALESCE(update_me.bio, users.bio)
      , visible_to = COALESCE(update_me.visible_to, users.visible_to)
    WHERE user_id = current_user_id()
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
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  BEGIN ATOMIC
    WITH jwt AS (
      SELECT get_facebook_id() AS facebook_id
    )
    , created AS (
      INSERT INTO users (facebook_id)
      SELECT facebook_id FROM jwt WHERE jwt.facebook_id IS NOT NULL
      EXCEPT
      SELECT facebook_id FROM users
      RETURNING user_id
    )
    SELECT user_id FROM users
    JOIN jwt USING (facebook_id)
    UNION ALL
    SELECT user_id FROM created;
  END;
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
GRANT SELECT, INSERT, UPDATE(name) ON woulds TO api;

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

CREATE FUNCTION public.restrict_custom_woulds() RETURNS TRIGGER
  LANGUAGE plpgsql AS $$DECLARE
    this_user_id bigint;
    num_recent bigint;
  BEGIN
    this_user_id := current_user_id();
    IF TG_OP = 'UPDATE' THEN
      IF EXISTS (
          SELECT FROM user_woulds uw
          WHERE would_id = NEW.would_id
          AND user_id <> this_user_id
        ) THEN
        RAISE EXCEPTION 'Cannot change the name of a column in use';
      END IF;
      RETURN NULL;
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

CREATE TRIGGER restrict_custom_woulds BEFORE INSERT OR UPDATE ON woulds
  FOR EACH ROW
  EXECUTE FUNCTION restrict_custom_woulds();

CREATE VIEW public.user_profiles AS
  SELECT
    users.user_id
  , users.facebook_id
  , users.name
  , users.bio
  , CASE
      WHEN users.user_id = current_user_id() THEN 'self'
      WHEN users.user_id IN (
        SELECT friend_id
        FROM facebook_friends f
        WHERE f.user_id = current_user_id()
      ) THEN 'friends'
      ELSE 'everyone'
    END::audience AS audience
  , fwu.since AS friends_since
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
    ON users.user_id = fwu.user_id
   AND fwu.friend_id = current_user_id()
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
  GROUP BY users.user_id, fwu.since;
GRANT SELECT ON user_profiles TO api;
