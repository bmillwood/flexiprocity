#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

tmpdir=$(mktemp -d)
trap 'rm -rfv "$tmpdir"' EXIT
d="$tmpdir"/flexiprocity
mkdir -p "$d"
mkdir -p "$d"/auth-server
cp auth-server/ubuntu/auth-server "$d"/auth-server/auth-server
mkdir -p "$d"/frontend
pushd frontend > /dev/null
cp *.{css,html} driver.js "$d"/frontend
elm make --optimize --output="$d"/elm.premin.js src/Main.elm
./version-info.sh > "$d"/frontend/version.js
uglifyjs "$d"/elm.premin.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output "$d"/frontend/elm.js
rm "$d"/elm.premin.js
popd > /dev/null
mkdir -p "$d"/nginx/var/{log,run}
cp nginx/{nginx.conf,start.sh} "$d"/nginx
sed -i -re 's/(listen .*)57958;/\180;/' -e 's/(listen .*)57959 ssl;/\1443 ssl;/' "$d"/nginx/nginx.conf
mkdir "$d"/postgraphile
cp postgraphile/{package.json,postgraphile.tags.json5,start.sh} "$d"/postgraphile

cat >> "$d"/start.sh <<'EOF'
#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
tmux new-session \; \
    split-window -h \; \
    send-keys "source secrets/secrets.env" Enter \
      "cd auth-server" Enter \
      "./auth-server" Enter \; \
    split-window -h \; \
    send-keys "cd nginx" Enter "nginx -p $PWD/nginx -c nginx.conf" Enter \; \
    split-window -h \; \
    send-keys "cd postgraphile" Enter \
      "npm install" Enter \
      "./start.sh prod" Enter \; \
    select-layout main-vertical \; \
    select-pane -t 0
EOF
chmod +x "$d"/start.sh
tar -C "$tmpdir" -czf flexiprocity.tar.gz flexiprocity
