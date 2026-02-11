import type { Uri } from "vscode";

import { Webview } from "vscode";
import type { InsightsData } from "../types";

const alphaNum =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
export function getNonce() {
  let text = "";
  for (let i = 0; i < 32; i++) {
    text += alphaNum.charAt(Math.floor(Math.random() * alphaNum.length));
  }
  return text;
}

export const head = (webview: Webview, stylePath: Uri, nonce: string) => {
  const csp = `default-src 'none'; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';`;

  return /* html */ `<head>
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta http-equiv="Content-Security-Policy" content="${csp}" />
    <meta charset="UTF-8" />
    <title>Clarity View</title>
    <link href="${webview.asWebviewUri(stylePath)}" rel="stylesheet" />
  </head>`;
};

export const emptyBody = () => /* html */ `<p>No insight to provide</p>`;

const typeSignature = (signature: any) =>
  /* html */ `<code>${
    typeof signature === "object"
      ? JSON.stringify(signature, null, 2)
      : signature
  }</code>`;

export const insightsBody = (insights: InsightsData) => {
  const { fnType, fnName, fnArgs, fnReturns } = insights;
  return /* html */ `
  ${
    fnType && fnName
      ? /* html */ `<h3><code>${fnType}</code> - <code>${fnName}</code></h3>`
      : ""
  }

  <h4>Arguments</h4>
  ${
    fnArgs.length > 0
      ? fnArgs
          .map(
            ({ name, signature }) =>
              /*html */ `<p>${name}: ${typeSignature(signature)}</p>`,
          )
          .join("")
      : /*html */ `<p>No args</p>`
  }

  <h4>Returns</h4>
  ${
    fnReturns
      ? /*html */ `<p>${typeSignature(fnReturns)}</p>`
      : /*html */ `<p>Returns nothing</p>`
  }

  <h4>Costs</h4>
  <table>
    <tr>
      <th>read count</th>
      <th>write count</th>
      <th>read length</th>
      <th>write length</th>
      <th>Runtime</th>
    </tr>
    <tr>
      <td>###</td>
      <td>###</td>
      <td>###</td>
      <td>###</td>
      <td>###</td>
    </tr>
  </table>
  `;
};
