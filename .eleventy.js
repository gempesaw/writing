const rssPlugin = require("@11ty/eleventy-plugin-rss");
const syntaxHighlight = require("@11ty/eleventy-plugin-syntaxhighlight");

const markdownIt = require("markdown-it");
const markdownItFootnote = require("markdown-it-footnote");

module.exports = function (eleventyConfig) {
  eleventyConfig.addPlugin(rssPlugin);
  eleventyConfig.addPlugin(syntaxHighlight);

  eleventyConfig.amendLibrary("md", (mdLib) =>
    mdLib.enable("code").use(markdownItFootnote),
  );

  // Split rendered HTML on the [[MORE]] marker for excerpts.
  // Falls back to the first <p> if no marker is present.
  // Source markdown is rendered once with all link refs intact, so
  // reference-style links in the excerpt resolve correctly.
  const MORE_RE = /<p>\s*(?:\[\[MORE\]\]|<!--\s*more\s*-->)\s*<\/p>/;
  eleventyConfig.addFilter("excerptOf", (html) => {
    if (!html) return "";
    if (MORE_RE.test(html)) return html.split(MORE_RE)[0];
    const match = html.match(/<p>[\s\S]*?<\/p>/);
    return match ? match[0] : "";
  });

  eleventyConfig.addPassthroughCopy("css");
  eleventyConfig.addPassthroughCopy("published/*.png");
  eleventyConfig.addPassthroughCopy("published/*.yaml");
  eleventyConfig.addPassthroughCopy("CNAME");

  eleventyConfig.ignores.add("drafts/**");
  eleventyConfig.ignores.add("ramen/**");
  eleventyConfig.ignores.add("theme.html");
  eleventyConfig.ignores.add("README.md");
  eleventyConfig.ignores.add("node_modules/**");

  eleventyConfig.addCollection("posts", (collection) =>
    collection
      .getFilteredByGlob("published/*.md")
      .sort((a, b) => b.date - a.date),
  );

  eleventyConfig.addCollection("postsByYear", (collection) => {
    const posts = collection
      .getFilteredByGlob("published/*.md")
      .sort((a, b) => b.date - a.date);
    const groups = {};
    for (const p of posts) {
      const year = String(new Date(p.date).getUTCFullYear());
      (groups[year] ||= []).push(p);
    }
    return Object.entries(groups)
      .sort(([a], [b]) => Number(b) - Number(a))
      .map(([year, posts]) => ({ year, posts }));
  });

  const slugify = (s) =>
    s
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, "-")
      .replace(/^-|-$/g, "");

  eleventyConfig.addFilter("tagSlug", slugify);

  eleventyConfig.addCollection("tagSlugs", (collection) => {
    const map = {};
    for (const post of collection.getFilteredByGlob("published/*.md")) {
      const raw = post.data.tags || [];
      for (const t of raw) {
        if (t === "posts") continue;
        const slug = slugify(t);
        if (!slug) continue;
        if (!map[slug]) map[slug] = { slug, label: t, posts: [] };
        if (!map[slug].posts.includes(post)) map[slug].posts.push(post);
      }
    }
    return Object.values(map)
      .map((entry) => ({
        ...entry,
        posts: entry.posts.sort((a, b) => b.date - a.date),
      }))
      .sort((a, b) => a.slug.localeCompare(b.slug));
  });

  eleventyConfig.addTransform("stripTumblrMarkers", function (content) {
    if ((this.page.outputPath || "").endsWith(".html")) {
      return content
        .replace(/<p>\s*\[\[MORE\]\]\s*<\/p>/g, "")
        .replace(/<p>\s*<!--\s*more\s*-->\s*<\/p>/g, "")
        .replace(/<!--\s*more\s*-->/g, "");
    }
    return content;
  });

  eleventyConfig.addFilter("readableDate", (date) =>
    new Date(date).toLocaleDateString("en-US", {
      year: "numeric",
      month: "long",
      day: "numeric",
    }),
  );

  eleventyConfig.addFilter("htmlDate", (date) =>
    new Date(date).toISOString().slice(0, 10),
  );

  eleventyConfig.addFilter("monthDay", (date) =>
    new Date(date).toISOString().slice(5, 10),
  );

  return {
    dir: {
      input: ".",
      output: "_site",
      includes: "_includes",
      data: "_data",
    },
    markdownTemplateEngine: false,
    htmlTemplateEngine: "njk",
    templateFormats: ["md", "njk", "html", "11ty.js"],
    pathPrefix: "/",
  };
};
