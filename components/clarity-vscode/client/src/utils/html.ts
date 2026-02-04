import type { Uri } from "vscode";

import { Webview } from "vscode";
import type { InsightsData, CostDetailsData } from "../types";

const alphaNum =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
export function getNonce() {
  let text = "";
  for (let i = 0; i < 32; i++) {
    text += alphaNum.charAt(Math.floor(Math.random() * alphaNum.length));
  }
  return text;
}

export const head = (
  webview: Webview,
  stylePath: Uri,
  nonce: string,
  additionalStyles?: string,
) => {
  const csp = `default-src 'none'; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';`;

  return /* html */ `<head>
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta http-equiv="Content-Security-Policy" content="${csp}" />
    <meta charset="UTF-8" />
    <title>Clarity View</title>
    <link href="${webview.asWebviewUri(stylePath)}" rel="stylesheet" />
    ${additionalStyles ? `<style>${additionalStyles}</style>` : ""}
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

export const costDetailsBody = (costDetails: CostDetailsData) => {
  const { function: functionName, cost, percentages, limits, trait_count } =
    costDetails;

  // Format full number with commas (e.g., 234,153 instead of 234.1k)
  const formatFullNumber = (num: number) => {
    return num.toLocaleString();
  };

  // Format abbreviated number for limits (e.g., 5B instead of 5000M, 15k instead of 15.0k)
  const formatAbbreviatedNumber = (num: number) => {
    if (num >= 1_000_000_000) {
      const value = num / 1_000_000_000;
      return `${value % 1 === 0 ? value.toFixed(0) : value.toFixed(1)}B`;
    }
    if (num >= 1_000_000) {
      const value = num / 1_000_000;
      return `${value % 1 === 0 ? value.toFixed(0) : value.toFixed(1)}M`;
    }
    if (num >= 1_000) {
      const value = num / 1_000;
      return `${value % 1 === 0 ? value.toFixed(0) : value.toFixed(1)}k`;
    }
    return num.toLocaleString();
  };

  // Format min-max range
  const formatRange = (min: number, max: number) => {
    if (min === max) {
      return formatFullNumber(max);
    }
    return `${formatFullNumber(min)} - ${formatFullNumber(max)}`;
  };

  const formatPercent = (percent: number) => {
    return `${percent.toFixed(2)}%`;
  };

  const getPercentColorClass = (percent: number) => {
    if (percent >= 90) return "percent-high";
    if (percent >= 70) return "percent-medium-high";
    if (percent >= 50) return "percent-medium";
    return "percent-low";
  };

  return /* html */ `
    <div class="cost-details-container">
      <h2>Cost Details: <code>${functionName}</code></h2>

      <div class="section">
        <h3>Trait Information</h3>
        <div class="info-box">
          <span class="label">Trait Count:</span>
          <span class="value">${trait_count}</span>
        </div>
      </div>

      <div class="section">
        <h3>Execution Cost</h3>
        <table class="cost-table">
          <thead>
            <tr>
              <th>Metric</th>
              <th>Value</th>
              <th>Percentage</th>
              <th>Limit</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td><strong>Runtime</strong></td>
              <td>${formatRange(cost.runtime.min, cost.runtime.max)}</td>
              <td class="${getPercentColorClass(percentages.runtime)}">
                ${formatPercent(percentages.runtime)}
              </td>
              <td>${formatAbbreviatedNumber(limits.runtime)}</td>
            </tr>
            <tr>
              <td><strong>Read Count</strong></td>
              <td>${formatRange(cost.read_count.min, cost.read_count.max)}</td>
              <td class="${getPercentColorClass(percentages.read_count)}">
                ${formatPercent(percentages.read_count)}
              </td>
              <td>${formatAbbreviatedNumber(limits.read_count)}</td>
            </tr>
            <tr>
              <td><strong>Read Length</strong></td>
              <td>${formatRange(cost.read_length.min, cost.read_length.max)}</td>
              <td class="${getPercentColorClass(percentages.read_length)}">
                ${formatPercent(percentages.read_length)}
              </td>
              <td>${formatAbbreviatedNumber(limits.read_length)}</td>
            </tr>
            <tr>
              <td><strong>Write Count</strong></td>
              <td>${formatRange(cost.write_count.min, cost.write_count.max)}</td>
              <td class="${getPercentColorClass(percentages.write_count)}">
                ${formatPercent(percentages.write_count)}
              </td>
              <td>${formatAbbreviatedNumber(limits.write_count)}</td>
            </tr>
            <tr>
              <td><strong>Write Length</strong></td>
              <td>${formatRange(cost.write_length.min, cost.write_length.max)}</td>
              <td class="${getPercentColorClass(percentages.write_length)}">
                ${formatPercent(percentages.write_length)}
              </td>
              <td>${formatAbbreviatedNumber(limits.write_length)}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <div class="section">
        <h3>Cost Breakdown</h3>
        <div class="breakdown">
          <div class="breakdown-item">
            <div class="breakdown-bar">
              <div
                class="breakdown-fill ${getPercentColorClass(percentages.runtime)}"
                data-width="runtime"
              ></div>
            </div>
            <span class="breakdown-label">Runtime: ${formatPercent(percentages.runtime)}</span>
          </div>
          <div class="breakdown-item">
            <div class="breakdown-bar">
              <div
                class="breakdown-fill ${getPercentColorClass(percentages.read_count)}"
                data-width="read-count"
              ></div>
            </div>
            <span class="breakdown-label">Read Count: ${formatPercent(percentages.read_count)}</span>
          </div>
          <div class="breakdown-item">
            <div class="breakdown-bar">
              <div
                class="breakdown-fill ${getPercentColorClass(percentages.read_length)}"
                data-width="read-length"
              ></div>
            </div>
            <span class="breakdown-label">Read Length: ${formatPercent(percentages.read_length)}</span>
          </div>
          <div class="breakdown-item">
            <div class="breakdown-bar">
              <div
                class="breakdown-fill ${getPercentColorClass(percentages.write_count)}"
                data-width="write-count"
              ></div>
            </div>
            <span class="breakdown-label">Write Count: ${formatPercent(percentages.write_count)}</span>
          </div>
          <div class="breakdown-item">
            <div class="breakdown-bar">
              <div
                class="breakdown-fill ${getPercentColorClass(percentages.write_length)}"
                data-width="write-length"
              ></div>
            </div>
            <span class="breakdown-label">Write Length: ${formatPercent(percentages.write_length)}</span>
          </div>
        </div>
      </div>
    </div>
  `;
};

export const costDetailsStyles = (
  percentages: CostDetailsData["percentages"],
) => {
  const runtimeWidth = Math.min(percentages.runtime, 100);
  const readCountWidth = Math.min(percentages.read_count, 100);
  const readLengthWidth = Math.min(percentages.read_length, 100);
  const writeCountWidth = Math.min(percentages.write_count, 100);
  const writeLengthWidth = Math.min(percentages.write_length, 100);

  return `
    .breakdown-fill[data-width="runtime"] { width: ${runtimeWidth}%; }
    .breakdown-fill[data-width="read-count"] { width: ${readCountWidth}%; }
    .breakdown-fill[data-width="read-length"] { width: ${readLengthWidth}%; }
    .breakdown-fill[data-width="write-count"] { width: ${writeCountWidth}%; }
    .breakdown-fill[data-width="write-length"] { width: ${writeLengthWidth}%; }
  `;
};
