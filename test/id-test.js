// Import the functions from the library
// Adjust the path based on where you saved the hyperbeam-id-generators.js file
import {
  generateCommitmentId,
  rsaid,
  hmacid,
  verifyCommitmentId,
} from "../src/id.js"

// Test data from the Erlang implementation
const testMessage = {
  commitments: {
    "PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE": {
      alg: "rsa-pss-sha512",
      "commitment-device": "httpsig@1.0",
      committer: "Tbun4iRRQW93gUiSAmTmZJ2PGI-_yYaXsX69ETgzSRE",
      signature:
        "http-sig-bba7e22451416f77=:d6cEnlw7JDqPRoMZj1R748tDu8S+8R2OYmHTOeBdyrHkhSGeUj7nGGA1K7pLKE79OSuqU9FmQdw1sRnI5u133Ryj4cge751gIy/DCkHN91Vujm5YXzcki9baTk6zXg18RhdVYvkJgo1aLUB9jDfPUUj4tgO8aOwRoeSFXn1P5aWY5jxa9dg6vRmZt5U+J3Xsj80eeSOGI1cC+PWHiw0nnMYPGRnFBp1zhNFdRH01JTsnbOHYHjD4PhQ17DePywTE2QRhrLSMH+XjrQoQA7hhvK0ezyGnRbByro1FH+n4TGPTnt8G+tvI7f4e24T50QKKlK3//ZyDumcHTnK0i5A2YDRXin9A4ngkXhmVxGgPjsgZSE8SfUd/D9U8SJqkuP71WbGbsfTer21P7AGC7xoloTV2nF0ZYHCN4Mlo2CH4WJqdFqlAvIhQPhuThwsSq84yp3e+W0P6+ejqwtQPcnLXDCHpk6bVQeNgA5a5+GLvnyPNo9REPp5cJ4yz5k/ESvPLnjElZlsMJ/h+BwJ8AyPDQdwz42Vnz9vrr7d6CWIFEKuV+Sjw/4ZHu2GEa8rUw1JmrMqlb21genHug8nOzFG6DyITJXBwzvnytsaYowauw8cgyRDxJ8p5pqCXwDz+8wnwnszXZquDDfQJZkNgzgt/UFXtyVfUaLB1Uc+4qh894Yw=:",
      "signature-input":
        'http-sig-bba7e22451416f77=("ao-types" "@path" "quantity" "recipient");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU"',
    },
    iK__u54zqDWrJpVVbf3uYvgvG1hxWZM6Tq1zusfGYIw: {
      alg: "hmac-sha256",
      "commitment-device": "httpsig@1.0",
      signature:
        "http-sig-bba7e22451416f77=:d6cEnlw7JDqPRoMZj1R748tDu8S+8R2OYmHTOeBdyrHkhSGeUj7nGGA1K7pLKE79OSuqU9FmQdw1sRnI5u133Ryj4cge751gIy/DCkHN91Vujm5YXzcki9baTk6zXg18RhdVYvkJgo1aLUB9jDfPUUj4tgO8aOwRoeSFXn1P5aWY5jxa9dg6vRmZt5U+J3Xsj80eeSOGI1cC+PWHiw0nnMYPGRnFBp1zhNFdRH01JTsnbOHYHjD4PhQ17DePywTE2QRhrLSMH+XjrQoQA7hhvK0ezyGnRbByro1FH+n4TGPTnt8G+tvI7f4e24T50QKKlK3//ZyDumcHTnK0i5A2YDRXin9A4ngkXhmVxGgPjsgZSE8SfUd/D9U8SJqkuP71WbGbsfTer21P7AGC7xoloTV2nF0ZYHCN4Mlo2CH4WJqdFqlAvIhQPhuThwsSq84yp3e+W0P6+ejqwtQPcnLXDCHpk6bVQeNgA5a5+GLvnyPNo9REPp5cJ4yz5k/ESvPLnjElZlsMJ/h+BwJ8AyPDQdwz42Vnz9vrr7d6CWIFEKuV+Sjw/4ZHu2GEa8rUw1JmrMqlb21genHug8nOzFG6DyITJXBwzvnytsaYowauw8cgyRDxJ8p5pqCXwDz+8wnwnszXZquDDfQJZkNgzgt/UFXtyVfUaLB1Uc+4qh894Yw=:",
      "signature-input":
        'http-sig-bba7e22451416f77=("ao-types" "@path" "quantity" "recipient");alg="rsa-pss-sha512";keyid="o1kvTqZQ0wbS_WkdwX70TFCk7UF76ldnJ85l8iRV7t6mSlzkXBYCecb-8RXsNEQQmO0KergtHOvhuBJmB6YXaYe_UftI_gendojfIa6jlTgw-qmH6g4_oErI8djDRbQSm-5nCfGVRuYxsNZLYDeqw4gFb9K3b1h7tuMoLd6-d5pkaLfTMUNcvs2OqpkLo0i_av746FieaURdWozwFqO0APtdA7pLHDqQZDMNdTmsUBJFszL6SOa1bKe5cUWnrq4uaW4NAN3JAQniILKGsKZENeKtfXwiKVaFJtriWWsbhOaNT0JLcuBAwXQAP59RXzcr8bRY6XFn8zBmEmZBGszOD9c9ssDENRFDa5uyVhk8XgIgQjErAWYd9T6edrYcIp3R78jhNK_nLiIBBz8_Oz3bLjL5i_aiV2gpfIbd44DCHihuuxSWRAPJxhEy9TS0_QbVOIWhcDTIeEJE3aRPTwSTMt1_Fec7i9HJWN0mvMbAAJw8k6HxjA3pFZiCowZJw7FBwMAeYgEwIeB82f-S2-PtFLwR9i0tExo36hEBHqaS4Y-O3NGgQ8mKnhT7Z1EfxEbA2BpR9oL8rJFEnPIrHHu7B88OHDDfnfRD3D79fKktnisC7XOuwbHG3TQo0_j4_mElH7xj_7IyAbmCUHDd-eRa482wOYXBB01DGnad901qaHU"',
    },
  },
  path: "credit-notice",
  quantity: 100,
  recipient: "Rix7e0HB-8OAaimcoYkxTZB-dStgTOHWUik1DvKD5vM",
}

// Message for HMAC computation (without commitments)
const hmacMessage = {
  "ao-types": 'quantity="integer"',
  path: "credit-notice",
  quantity: "100",
  recipient: "Rix7e0HB-8OAaimcoYkxTZB-dStgTOHWUik1DvKD5vM",
  signature:
    testMessage.commitments["PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE"]
      .signature,
  "signature-input":
    testMessage.commitments["PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE"][
      "signature-input"
    ],
}

console.log("=== HyperBEAM ID Generator Test Suite ===\n")

// Test 1: RSA Commitment ID
console.log("Test 1: RSA-PSS Commitment ID Generation")
const rsaCommitment =
  testMessage.commitments["PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE"]
const rsaId = rsaid(rsaCommitment)
console.log("Expected ID: PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE")
console.log("Generated ID:", rsaId)
console.log(
  "Test passed:",
  rsaId === "PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE"
)

// Test 2: HMAC Commitment ID
console.log("\nTest 2: HMAC Commitment ID Generation")
const hmacId = hmacid(hmacMessage)
console.log("Expected ID: iK__u54zqDWrJpVVbf3uYvgvG1hxWZM6Tq1zusfGYIw")
console.log("Generated ID:", hmacId)
console.log(
  "Test passed:",
  hmacId === "iK__u54zqDWrJpVVbf3uYvgvG1hxWZM6Tq1zusfGYIw"
)

// Test 3: Generic commitment ID function
console.log("\nTest 3: Generic Commitment ID Function")
const genericRsaId = generateCommitmentId(rsaCommitment)
console.log("RSA via generic function:", genericRsaId)
console.log(
  "RSA test passed:",
  genericRsaId === "PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE"
)

const hmacCommitment = { alg: "hmac-sha256" }
const genericHmacId = generateCommitmentId(hmacCommitment, hmacMessage)
console.log("HMAC via generic function:", genericHmacId)
console.log(
  "HMAC test passed:",
  genericHmacId === "iK__u54zqDWrJpVVbf3uYvgvG1hxWZM6Tq1zusfGYIw"
)

// Test 4: Verify commitment IDs
console.log("\nTest 4: Verify Commitment IDs")
const rsaVerified = verifyCommitmentId(
  rsaCommitment,
  "PDw08yIW-ImV2N3ItIlWQ1wBt2t_H1CGMUOxJjx47HE"
)
console.log("RSA verification:", rsaVerified)

const hmacVerified = verifyCommitmentId(
  hmacCommitment,
  "iK__u54zqDWrJpVVbf3uYvgvG1hxWZM6Tq1zusfGYIw",
  hmacMessage
)
console.log("HMAC verification:", hmacVerified)

// Test 5: Demonstrate HMAC independence from signature
console.log("\nTest 5: HMAC Independence from RSA Signature")
const modifiedMessage = {
  ...hmacMessage,
  signature:
    "http-sig-bba7e22451416f77=:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=:",
}
const hmacIdModified = hmacid(modifiedMessage)
console.log("HMAC with different RSA signature:", hmacIdModified)
console.log(
  "Still the same ID:",
  hmacIdModified === "iK__u54zqDWrJpVVbf3uYvgvG1hxWZM6Tq1zusfGYIw"
)

console.log("\n=== All Tests Completed ===")
console.log("The HMAC ID is deterministic and depends only on message content,")
console.log("not on the RSA signature value.")
