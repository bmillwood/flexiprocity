{ lib }:
{
  services.postgresql = {
    enable = true;
    ensureUsers = [
      { name = "api"; ensureClauses.login = true; }
      { name = "inbox"; ensureClauses.login = true; }
      { name = "meddler"; ensureClauses.login = true; }
    ];
    identMap = ''
      local api api
      local api inbox
      local api meddler
      local postgres postgres
    '';
    authentication = lib.mkForce ''
      local all all peer map=local
    '';
    initialScript = ./structure.sql;
  };
}
