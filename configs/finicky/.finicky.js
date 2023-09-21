// Use https://finicky-kickstart.now.sh to generate basic configuration
// Learn more about configuration options: https://github.com/johnste/finicky/wiki/Configuration

module.exports = {
  defaultBrowser: "Firefox",
  handlers: [
    {
      // Open work domains in Google Chrome
      match: [
        "localhost*",
        "afilio.de/*",
        "google.com/*", // match google.com urls
        finicky.matchDomains(/.*\.google.com/), // use helper function to match on domain only
        finicky.matchDomains(/app\.graphite\.dev/), // use helper function to match on domain only
        "graphite.dev/*",
        "afilio.sentry.io/*",
        "temporal.io/*",
        "cloud.temporal.io/*",
        "pulumi.com/*",
        "app.pulumi.com/*",
        "personio.de/*",
        "peerdom.org/*",
      ],
      browser: "Google Chrome",
    },
    {
      match: [/slack:\/\/channel/, "slack.com/*", "afilio-de.slack.com/*"],
      browser: "/Applications/Slack.app",
    },
    {
      match: /figma\.com\/file/,
      browser: "/Applications/Figma.app",
    },
    {
      match: /notion\.so/,
      browser: "/Applications/Notion.app",
    },
  ],
};
