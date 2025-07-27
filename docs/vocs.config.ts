import { defineConfig } from 'vocs'

export default defineConfig({
  iconUrl: '/favicon.ico',
  title: 'WAO',
  ogImageUrl: 'https://vocs.dev/api/og?logo=%logo&title=%title&description=%description',
  topNav: [ 
    { text: 'Get Started', link: '/getting-started' },
    { text: 'HyperBEAM', link: '/hyperbeam/decoding-from-scratch' }, 
    { text: 'API', link: '/api/ao' }, 
  ],   
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
      text: 'WAO Hub',
      link: '/hub',
    },
    {
      text: 'HyperBEAM',
      collapsed: false,
      items: [
	{
          text: 'Decoding from Scratch',
          link: '/hyperbeam/decoding-from-scratch',
        },
	{
          text: 'Installing HB and WAO',
          link: '/hyperbeam/installing-hb-wao',
        },
	{
          text: 'Devices and Pathing',
          link: '/hyperbeam/devices-pathing'
        },
	{
          text: 'Custom Devices and Codecs',
          link: '/hyperbeam/custom-devices-codecs'
        },
	{
          text: 'Flat Codec',
          link: '/hyperbeam/codec-flat'
        },
	{
          text: 'Structured Codec',
          link: '/hyperbeam/codec-structured'
        },
	{
          text: 'Httpsig Codec',
          link: '/hyperbeam/codec-httpsig'
        },
	{
          text: 'Http Message Signatures',
          link: '/hyperbeam/http-message-signatures'
        },
	{
          text: 'Hashpaths',
          link: '/hyperbeam/hashpaths'
        },
	{
          text: 'Device Composition',
          link: '/hyperbeam/device-composition'
        },
	{
          text: 'Processes and Scheduler',
          link: '/hyperbeam/processes-scheduler'
        },
	{
          text: 'Legacynet Compatible AOS',
          link: '/hyperbeam/legacynet-aos'
        },
	{
          text: 'Payment System',
          link: '/hyperbeam/payment-system'
        },
      ],
    },
    {
      text: 'Tutorials',
      collapsed: false,
      items: [
	{
          text: 'Legacynet AOS on HyperBEAM',
          link: '/tutorials/legacynet-aos',
        },
	{
          text: 'Mainnet AOS',
          link: '/tutorials/mainnet-aos',
        },
	{
          text: 'Creating Custom Devices',
          link: '/tutorials/creating-devices',
        },
	{
          text: 'Custom Devices in Rust',
          link: '/tutorials/devices-rust',
        },
	{
          text: 'Custom Devices in C++',
          link: '/tutorials/devices-cpp',
        },
	{
          text: 'Running LLMs on AOS',
          link: '/tutorials/running-llms',
        },
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
        },
	{
          text: 'HBSig',
          link: '/api/hbsig',
        }
      ],
    },
  ],
})
