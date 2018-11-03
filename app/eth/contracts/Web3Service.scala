package eth.contracts

import org.web3j.protocol.http.HttpService

class Web3Service {

  val service = new HttpService("https://mainnet.infura.io/8rldhike5t6EoXQpanE5 ")

  val web3 = org.web3j.protocol.Web3j.build(service)

}