local locale_gen_cmds_default(cmds) = cmds + [
  'echo "en_US.UTF-8 UTF-8" > /etc/locale.gen',
  'locale-gen',
];

local locale_gen_cmds_ubuntu1204(cmds) = cmds + [
  'locale-gen en_US.UTF-8',
];

local linuxbrew_debian_cmds(cmds) = [
  'apt-get update',
  'apt-get install --yes build-essential curl file git',
] + cmds;

local test_step(emacs_ver) = {
  name: 'test-emacs%s' % emacs_ver,
  image: 'silex/emacs:%s-dev' % emacs_ver,
  commands: [
    'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"',
    'cask install',
    'sleep 15',
    # Waiting for httpbin
    'cask exec ert-runner',
  ],
  volumes: [
    {
      name: 'locales',
      path: '/usr/lib/locale',
    },
    {
      name: 'linuxbrew',
      path: '/home/linuxbrew/.linuxbrew',
    },
  ],
  environment: {
    MB_URL_TEST__HTTPBIN_PREFIX: 'http://httpbin',
  },
  depends_on: [
    'install ci deps',
  ],
};

local generate_pipeline(args) = {
  kind: 'pipeline',
  name: args.pipeline_name,
  services: [
    {
      name: 'httpbin',
      image: 'kennethreitz/httpbin',
    },
  ],
  steps: [
    {
      # We have to generate `en_US.UTF-8` locale because brew sets `LC_ALL` to
      # it.
      name: 'install locales',
      image: args.linuxbrew_image,
      commands: args.locale_gen_cmds_func([
        'apt-get update',
        'apt-get install --yes locales',
      ]),
      volumes: [
        {
          name: 'locales',
          path: '/usr/lib/locale',
        },
      ],
    },
    {
      name: 'install Linuxbrew',
      image: args.linuxbrew_image,
      commands: [
        'apt-get update',
        'apt-get install --yes git',
        'git clone https://github.com/Homebrew/brew /home/linuxbrew/.linuxbrew/Homebrew',
        'mkdir -p /home/linuxbrew/.linuxbrew/bin',
        'ln -s ../Homebrew/bin/brew /home/linuxbrew/.linuxbrew/bin',
      ],
      volumes: [
        {
          name: 'linuxbrew',
          path: '/home/linuxbrew/.linuxbrew',
        },
      ],
    },
    {
      name: 'install ci deps',
      image: args.linuxbrew_image,
      commands: args.ci_deps_cmds_func([
        'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"',
        'brew update',
        'brew bundle',
      ]),
      volumes: [
        {
          name: 'locales',
          path: '/usr/lib/locale',
        },
        {
          name: 'linuxbrew',
          path: '/home/linuxbrew/.linuxbrew',
        },
        {
          name: 'cache',
          path: '/root/.cache',
        },
      ],
      environment: {
        HOMEBREW_DEVELOPER: 1,
        HOMEBREW_NO_AUTO_UPDATE: 1,
        HOMEBREW_NO_ANALYTICS: 1,
        HOMEBREW_NO_INSTALL_CLEANUP: 1,
      },
      depends_on: [
        'install locales',
        'install Linuxbrew',
      ],
    },
  ] + std.map(test_step, args.emacs_vers),
  volumes: [
    {
      name: 'cache',
      temp: {},
    },
    {
      name: 'locales',
      temp: {},
    },
    {
      name: 'linuxbrew',
      temp: {},
    },
  ],
};

std.map(generate_pipeline, [
  {
    pipeline_name: 'default',
    linuxbrew_image: 'buildpack-deps:stable',
    locale_gen_cmds_func: locale_gen_cmds_default,
    ci_deps_cmds_func: std.prune,
    emacs_vers: ['24.5', '25.1', '25.2', '25.3'],
  },
  {
    # According to [1] and [2], Emacs 24.4 cannot be built on Ubuntu 18.04, so
    # `silex/emacs:24.4` use Ubuntu 12.04 as its base image.  We have to
    # install dependencies on Ubuntu 12.04.
    #
    # [1]: https://github.com/Silex/docker-emacs/issues/34
    # [2]: https://github.com/Silex/docker-emacs/commit/df66168dc4edc5a746351685b88ac59d3efcb183
    pipeline_name: 'test for emacs 24.4',
    linuxbrew_image: 'ubuntu:12.04',
    locale_gen_cmds_func: locale_gen_cmds_ubuntu1204,
    ci_deps_cmds_func: linuxbrew_debian_cmds,
    emacs_vers: ['24.4'],
  },
])
