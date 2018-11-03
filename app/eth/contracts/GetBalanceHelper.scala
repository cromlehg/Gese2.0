package eth.contracts

import java.math.BigDecimal

import org.web3j.protocol.core.DefaultBlockParameterName

class GetBalanceHelper extends Web3Service {

  def getBalance(invokerAddress: String): String = {
    val web3Balance = web3.ethGetBalance(invokerAddress, DefaultBlockParameterName.LATEST).send()
    new BigDecimal(web3Balance.getBalance.toString).divide(new BigDecimal("1000000000000000000")).toString
  }

}

class GetBalance {

  def apply(invokerAddress: String): String =
    new GetBalanceHelper().getBalance(invokerAddress)

}