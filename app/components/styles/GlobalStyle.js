export default function GlobalStyle() {
  return (
    <style jsx global>{`
      body {
        background: white;
        color: #222;
      }
      .markdown-body nav[style*="justify-content:space-between"] {
        display: flex;
        justify-content: space-between;
        gap: 1rem;
        flex-wrap: nowrap;
      }

      .markdown-body nav[style*="justify-content:space-between"] > a {
        width: calc(50% - 0.5rem);
        padding: 0.75em 2em;
        border: 1px solid #ccc;
        border-radius: 6px;
        text-decoration: none;
        background: #fff;
        box-sizing: border-box;
        font-weight: 500;
        transition: all 0.2s ease;
        overflow: hidden;
        white-space: nowrap;
        text-overflow: ellipsis;
      }

      .markdown-body
        nav[style*="justify-content:space-between"]
        > a:first-of-type {
        text-align: left;
      }

      .markdown-body
        nav[style*="justify-content:space-between"]
        > a:last-of-type {
        text-align: right;
      }

      .markdown-body
        nav[style*="justify-content:space-between"]
        > a:first-of-type:not(:empty)::before {
        content: "Previous";
        display: block;
        font-size: 0.75em;
        color: #888;
        margin-bottom: 0.25em;
        text-align: left;
      }

      .markdown-body
        nav[style*="justify-content:space-between"]
        > a:last-of-type:not(:empty)::before {
        content: "Next";
        display: block;
        font-size: 0.75em;
        color: #888;
        margin-bottom: 0.25em;
        text-align: right;
      }

      .markdown-body nav[style*="justify-content:space-between"] > a:empty {
        opacity: 0;
        pointer-events: none;
        border: none;
        background: transparent;
      }

      .markdown-body
        nav[style*="justify-content:space-between"]
        > a:not(:empty):hover {
        box-shadow: 0 0 0 1px #0969da80;
      }

      * {
        scrollbar-width: thin;
        scrollbar-color: #bbbbbb #f1f1f1;
      }

      *::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }

      *::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 4px;
      }

      *::-webkit-scrollbar-thumb {
        background: #bbbbbb;
        border-radius: 4px;
      }

      *::-webkit-scrollbar-thumb:hover {
        background: #999999;
      }
    `}</style>
  )
}
