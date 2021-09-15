local test_step(ci_deps_cmds) = function(emacs_ver) {
  name: 'test-emacs%s' % emacs_ver,
  image: 'silex/emacs:%s-ci-cask' % emacs_ver,
  commands: ci_deps_cmds + [
    'cask install',
    'sleep 15',
    // Waiting for httpbin
    'cask exec ert-runner',
  ],
  environment: {
    MB_URL_TEST__HTTPBIN_PREFIX: 'http://httpbin',
  },
  depends_on: [
    'no-op',
  ],
};

[
  {
    kind: 'pipeline',
    type: 'docker',
    name: 'default',
    services: [
      {
        name: 'httpbin',
        image: 'kennethreitz/httpbin',
      },
    ],
    steps: [
      {
        // For parallel steps
        name: 'no-op',
        image: 'hello-world:linux',
      },
    ] + std.map(test_step([
      'apt-get update && apt-get --yes install curl httpie',
    ]), ['24', '25', '26']),
  },
]
