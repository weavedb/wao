let global = {
  dryrun: true,
  getWallet: async () => {
    if (!global.walletRef.current) return null
    arweaveWallet.connect(
      ["ACCESS_ADDRESS", "SIGN_TRANSACTION", "ACCESS_PUBLIC_KEY"],
      {
        name: "WAO LOCALNET",
      }
    )
    const userAddress = await arweaveWallet.getActiveAddress()
    if (global.walletRef.current.address !== userAddress) {
      return null
    } else {
      return userAddress ? arweaveWallet : null
    }
  },
}

export default global
