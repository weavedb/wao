import { defineConfig } from 'vocs'

export default defineConfig({
  title: 'WAO',
    socials: [
    {
      icon: 'discord',
      link: 'https://discord.gg/vCkuVhkugY',
    },
    {
      icon: 'github',
      link: 'https://github.com/weavedb/wao',
    },
    {
      icon: 'x',
      link: 'https://twitter.com/WaoEco',
    },
  ],
  sidebar: [
    {
      text: 'Getting Started',
      link: '/getting-started',
    },
    {
      text: 'HyperBEAM',
      link: '/hyperbeam',
    },
    {
      text: 'Legacynet AOS',
      link: '/legacynet',
    },
    {
      text: 'AO The Web',
      link: '/web',
    },
    {
      text: 'Tutorials',
      collapsed: false,
      items: [
      ],
    },
    {
      text: 'API',
      collapsed: false,
      items: [
        {
          text: 'AO',
          link: '/api/ao',
        },
	{
          text: 'Process',
          link: '/api/process',
        },
	{
          text: 'Function Piping',
          link: '/api/function-piping',
        },
	{
          text: 'AR',
          link: '/api/ar',
        },
	{
          text: 'GQL',
          link: '/api/gql',
        },
	{
          text: 'ArMem',
          link: '/api/armem',
        },
	{
          text: 'HB',
          link: '/api/hb',
        },
	{
          text: 'HyperBEAM',
          link: '/api/hyperbeam',
        }
      ],
    },
  ],
})
