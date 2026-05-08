module.exports = {
  layout: "post.njk",
  permalink: (data) => `/post/${data.page.fileSlug}/`,
  tags: ["posts"],
};
