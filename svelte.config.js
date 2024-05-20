import preprocess from "svelte-preprocess";
import adapter from "@sveltejs/adapter-static";
import { vitePreprocess } from "@sveltejs/kit/vite";
import { mdsvex } from "mdsvex";
import { defineMDSveXConfig as defineConfig } from "mdsvex";
import shiki from 'shiki'
import rehypeSlug from "rehype-slug"
import rehypeAutolinkHeadings from "rehype-autolink-headings"
import remarkMath from 'remark-math'
import rehypeKatexSvelte from 'rehype-katex-svelte'

function addSlashes(str) {
  return str.replace(/[\\"'`$]/g, '\\$&');
}

const mdsvexConfig = defineConfig({
  extensions: [".svelte.md", ".md", ".svx"],

  smartypants: {
    dashes: "oldschool",
  },
  
  remarkPlugins: [
    remarkMath,
  ],

  rehypePlugins: [
    rehypeSlug,
    [rehypeAutolinkHeadings, { behavior: 'wrap' }],
    [rehypeKatexSvelte, {
      macros: {
        '\\Gal': '\\operatorname{Gal}',
        '\\Stab': '\\operatorname{Stab}',
        '\\mcO': '\\mathcal{O}',
      }
    }],
  ],

  highlight: {
    highlighter: async (code, lang) => {
      const hl = await shiki.getHighlighter({ theme: 'nord' })
      
      try {
        const tokens = hl.codeToThemedTokens(code, lang)

        const html = shiki.renderToHtml(tokens, {
          elements: {
            pre: ({ className, style, children }) => {
              return `<pre> {@html \`${addSlashes(children)}\`} </pre>`
            }
          }
        })
        
        return html
      } catch (err) {
        console.log(err)
        return `<pre><code>${code}</code></pre>`
      }
    }
  }
});

/** @type {import('@sveltejs/kit').Config} */
const config = {
  extensions: [".svelte", ...mdsvexConfig.extensions],

  // Consult https://kit.svelte.dev/docs/integrations#preprocessors
  // for more information about preprocessors
  preprocess: [
    mdsvex(mdsvexConfig),
    vitePreprocess(),
    preprocess({
      scss: {
        prependData: '@use "src/variables.scss" as *;',
      },
    }),
  ],

  kit: {
    // adapter-auto only supports some environments, see https://kit.svelte.dev/docs/adapter-auto for a list.
    // If your environment is not supported or you settled on a specific environment, switch out the adapter.
    // See https://kit.svelte.dev/docs/adapters for more information about adapters.
    adapter: adapter(),
  },
};

export default config;
