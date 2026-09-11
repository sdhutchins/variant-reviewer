// Copy only the rendered body of the text card containing the clicked button.
// Keeping this client-side avoids duplicating each card's formatted content in R.
window.vrCopyCardText = async function (button) {
  const cardBody = button.closest(".card")?.querySelector(".card-body");
  const status = button.parentElement.querySelector(".vr-copy-status");
  const text = cardBody?.innerText.trim();
  const label = button.querySelector(".vr-copy-label");
  const defaultIcon = button.querySelector(".vr-copy-icon-default");
  const successIcon = button.querySelector(".vr-copy-icon-success");
  const errorIcon = button.querySelector(".vr-copy-icon-error");
  const defaultDescription = button.dataset.copyLabel;

  const resetButton = function () {
    button.classList.remove("btn-success", "btn-outline-danger");
    button.classList.add("btn-outline-secondary");
    button.title = defaultDescription;
    button.setAttribute("aria-label", defaultDescription);
    label.textContent = "Copy";
    defaultIcon.classList.remove("d-none");
    successIcon.classList.add("d-none");
    errorIcon.classList.add("d-none");
  };

  const showResult = function (succeeded) {
    const message = succeeded ? "Copied." : "Could not copy. Try again.";

    button.classList.remove("btn-outline-secondary");
    button.classList.add(succeeded ? "btn-success" : "btn-outline-danger");
    button.title = message;
    button.setAttribute("aria-label", message);
    label.textContent = succeeded ? "Copied" : "Try again";
    defaultIcon.classList.add("d-none");
    successIcon.classList.toggle("d-none", !succeeded);
    errorIcon.classList.toggle("d-none", succeeded);
    status.textContent = "";
    window.requestAnimationFrame(function () {
      status.textContent = message;
    });

    window.clearTimeout(button.vrCopyResetTimer);
    button.vrCopyResetTimer = window.setTimeout(resetButton, 2000);
  };

  if (!text || !navigator.clipboard) {
    showResult(false);
    return;
  }

  try {
    await navigator.clipboard.writeText(text);
    showResult(true);
  } catch (error) {
    showResult(false);
  }
};
