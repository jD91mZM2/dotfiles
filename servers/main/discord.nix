{ inputs, system, ... }:
let
  abottomod = inputs.abottomod.defaultPackage."${system}";
  timeywimey = inputs.timeywimey.defaultPackage."${system}";
  schedulebot = inputs.schedulebot.defaultPackage."${system}";
in
{
  deployment.secrets =
    let
      makeKey = source: {
        keySource = source;
      };
    in
    {
      discord-abottomod = {
        keySource = "discord/abottomod";
        owner.user = "abottomod";
      };
      discord-timeywimey = {
        keySource = "discord/timeywimey";
        owner.user = "timeywimey";
      };
      discord-schedulebot = {
        keySource = "discord/schedulebot";
        owner.user = "schedulebot";
      };
    };

  # Services
  custom.services =
    let
      normal = script: {
        inherit script;
      };
    in
    {
      abottomod = normal "ABOTTOMOD_TOKEN=\"$(cat /var/lib/secrets/discord-abottomod)\" ${abottomod}/bin/abottomod";
      timeywimey = normal "TIMEYWIMEY_TOKEN=\"$(cat /var/lib/secrets/discord-timeywimey)\" ${timeywimey}/bin/timeywimey";
      schedulebot = normal "SCHEDULEBOT_TOKEN=\"$(cat /var/lib/secrets/discord-schedulebot)\" ${schedulebot}/bin/schedulebot";
    };
}
