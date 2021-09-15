local test_step(ci_deps_cmds) = function(emacs_ver) {
  name: 'test-emacs%s' % emacs_ver,
  image: 'silex/emacs:%s-ci-eldev' % emacs_ver,
  commands: ci_deps_cmds + [
    'sleep 15',
    // Waiting for httpbin
    'eldev lint',
    'eldev test',
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
    name: 'Mega-Linter',
    workspace: {
      path: '/drone/src',
    },
    steps: [
      {
        name: 'Lint',
        image: 'nvuillam/mega-linter:v4',
        environment: {
          DEFAULT_WORKSPACE: '/drone/src',
        },
      },
    ],
  },
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
